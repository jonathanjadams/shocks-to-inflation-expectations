%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%           Behavioral Expectations Equilibrium Toolkit
%             Jonathan J. Adams (jonathanjadams.com)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BEET_solve: this program solves a model with behavioral expectations or
% sentiment shocks

% Version 0.2 (2024/4/30)

% dependencies: Uhlig Toolkit subroutines (add to path), GENSYS functions (optional, add to path), BEET_foreterm (optional)

% inputs: 
%  -  **AA_fire**, **BB_fire**,... **NN_fire** (*Required*): matrices which encode the rational expectations model in "Uhlig form" (corresponding to matrices $A,B,...N$ respectively in Uhlig's notation)
%  - **BE_phivec**: vector of coefficients $[\phi_0,\phi_1,...,\phi_J]$ that encode a deterministic behaiovral expectations operator $\mathbb{E}^k$ in terms of current and past rational forecasts:
%    $$\mathbb{E}^k_t[x_{t+1}] = \sum_{j=0}^{J}\phi_j \mathbb{E}^k_{t-j}[x_{t+1}]$$
%  - **senti_exovars**: vector of indices identifying the exogenous variables about which agents have stochastic belief distortions or "sentiments"
%  - **senti_endovars**: vector of indices identifying the endogenous variables about which agents have stochastic belief distortions or "sentiments"
%  - **fcast_vars**: vector of indices identifying the endogenous variables whose one-period-ahead forecasts should be added to the model as additional endogenous variables.  This will be done automatically for variables identified in **senti_endovars**, even if they are not specified here.
%  - **fcast_hors**: matrix identifying variables whose many-periods-ahead forecasts should be added to the model as additional endogenous variables.  If there are $n$ such forecasts to be added, **fcast_hors** is specified as an $n\times 3$ matrix.  In each row, the first entry indexes the endogenous variable to be forecast, the second entry indexes the forecast horizon (an integer number of periods), and the third entry is set to $1$ if the forecast is cumulative and $0$ otherwise.
%  - **BEET_method**: scalar identifying which method should be used to solve the model.  $0$ indicates Uhlig (the default), $1$ indicates GENSYS (*still experimental*), $2$ indicates both (for diagnostic purposes)

n_exo_fire = size(NN_fire,1); %number of exogenous states in the FIRE base model (sometimes different than n_exo if expectations are behavioral)
n_fl = size(GG_fire,1);  %number of forward looking equations in base model
if ~exist('CC_fire','var')
    CC_fire = [];
    AA_fire = zeros(0,n_fl);
    BB_fire = AA_fire;
    DD_fire = zeros(0,n_exo_fire);
    JJ_fire = zeros(n_fl,0);
    KK_fire = zeros(n_fl,0);
end
n_con = size(CC_fire,1); %number of contemporaneous equations in base model
if ~exist('BE_phivec','var') %if no behavioral expectation is specified, set expanded L,M,N matrices to be FIRE matrices
     DD_firebig = DD_fire;
    LL_firebig = LL_fire;
    MM_firebig = MM_fire;
    NN_firebig = NN_fire;
else 
    %but if there are behavioral expectations, we will need to redefine the
    %state vector to include lags of states:
    NN_firebig = repmat(0*NN_fire,length(BE_phivec));
    NN_firebig(1:size(NN_fire,1),1:size(NN_fire,2))=NN_fire;
    NN_firebig(1+size(NN_fire,1):end,1:size(NN_fire,2)*(length(BE_phivec)-1)) = eye((length(BE_phivec)-1)*size(NN_fire,1));
    DD_firebig = [DD_fire, repmat(0*DD_fire,1,(length(BE_phivec)-1))];
    LL_firebig = [LL_fire, repmat(0*LL_fire,1,(length(BE_phivec)-1))];
    MM_firebig = [MM_fire, repmat(0*MM_fire,1,(length(BE_phivec)-1))];
    %And then define this misperceived law of motion:
    NN_misp = NN_firebig;
    NN_misp(1:size(NN_fire,1),:) = kron(BE_phivec,NN_fire);
end
n_exo = size(NN_firebig,1); %number of exogenous states in the (modified) base model

if ~exist('senti_endovars','var')
    senti_endovars = [];
end
if ~exist('senti_exovars','var')
    senti_exovars = [];
end


if ~(size(CC_fire,1)==size(CC_fire,2) && size(GG_fire,1)==size(GG_fire,2) && size(NN_fire,1)==size(NN_fire,2))
    di('C, G, and N matrices need to be square')
end
if ~exist('fcast_vars','var')
   disp('Warning: no forecast dimensions specified') 
   disp('... so I have reset forecast dimensions to match your stated endogenous sentiment dimensions') 
    fcast_vars=senti_endovars;
end
if ~isempty(setdiff(senti_endovars, fcast_vars))
   disp('Warning: endogenous sentiment dimensions are not a subset of forecast dimensions') 
   disp('... so I have reset forecast dimensions to match your stated endogenous sentiment dimensions') 
   fcast_vars=senti_endovars;
end
if size(fcast_vars,2)>1
    fcast_vars = fcast_vars'; % if fcast_vars is a row, make it a column
    if size(fcast_vars,2)>1
       disp('Warning: fcast_vars need to be a vector. Start over!')  
       stop
    end
end
if exist('fcast_hors','var')
    if isempty(fcast_hors)
    else
        if ~isempty(setdiff(fcast_hors(:,1), fcast_vars))
           disp('Warning: term structure forecast dimensions are not a subset of forecast dimensions') 
           disp('... so I have reset forecast dimensions to include your requested term structure forecast dimensions') 
           fcast_vars=[fcast_vars; setdiff(fcast_hors(:,1), fcast_vars)];
        end
    end
end

if max(senti_exovars) > n_exo
   disp('Warning: exogenous sentiment dimensions are not a subset of exogenous state dimensions')
   disp('... so I have reset exogenous sentiment dimensions to match the exogenous state dimensions')    
   senti_exovars = 1:n_exo;
end

%No sentiment autocorrelation declared?
if ~exist('senti_autocorr','var')
    senti_autocorr = 0; %.... then set it to zero
end


%matrix chooses which forward looking variables (the "x"s) will be forecasted (fcast_vars picks vector entries)
nonfcast_vars = setdiff(1:n_fl,fcast_vars); 
choose_forecasts = zeros(length(fcast_vars),n_fl);
for jj = 1:length(fcast_vars)
    choose_forecasts(jj,fcast_vars(jj))=1;
end
n_f = length(fcast_vars); %number of forecasts to include

%we are going to introduce sentiments, which either affect forecasts of
%endogenous variables or forecasts of exog states
%exog state entries are [exog s_exo s_endo]

%n_s_exo = 3; %number of sentiments of exogenous states to include
%s_exo_mat = zeros(n_exo,n_s_exo);  %identifies which exogenous state is associated with the sentiment
%s_exo_mat(1,1)=1; s_exo_mat(2,2)=1; s_exo_mat(3,3)=1;
n_s_exo = length(senti_exovars); %number of sentiments of endogenous states to include
s_exo_mat = zeros(n_exo,n_s_exo);
choose_senti_exo = zeros(length(senti_exovars),n_exo);
for jj = 1:length(senti_exovars)
    choose_senti_exo(jj,senti_exovars(jj))=1;
end



n_s_endo = length(senti_endovars); %number of sentiments of endogenous states to include
%s_endo_mat = zeros(n_exo,n_s_endo);
choose_senti_vars = zeros(length(senti_endovars),n_fl);
for jj = 1:length(senti_endovars)
    choose_senti_vars(jj,senti_endovars(jj))=1;
end
choose_fcasts_senti=choose_forecasts*choose_senti_vars'; %identifies which forecasts (row) have a sentiment (column)

n_senti = n_s_exo + n_s_endo;
NN_state_senti = [choose_senti_exo', zeros(n_exo,n_s_endo)];

%%%%
%  construct modified matrices
%%%%

%first set of matrices for non-expectational eqn (taylor rule)
AA = [AA_fire zeros(n_con,n_f)];
BB = [BB_fire zeros(n_con,n_f)];
CC = CC_fire;
DD = [DD_firebig zeros(n_con,n_senti)];

%second set for expectational eqn (ordered Euler, NKPC):
FF_forecasts = [choose_forecasts, zeros(n_f)]; %forecast variables forecast endogenous variables selected by choose_forecasts
FF_eulers = zeros(n_fl,n_fl+n_f); 
FF_eulers(:,nonfcast_vars)=FF_fire(:,nonfcast_vars); %non-forecasted variables are one-period-ahead as normal
FF = [FF_eulers; FF_forecasts];
GG_forecasts = [zeros(n_f,n_fl) -eye(n_f)]; %forecast variables match up to expectations of vars in FF_forecasts
GG_eulers = [GG_fire zeros(n_fl,n_f)]; 
GG_eulers(:,n_fl+1:end)=FF_fire(:,fcast_vars); %replacing forecasted variables with their included forecasts
GG = [GG_eulers; GG_forecasts];
HH = [HH_fire zeros(n_fl,n_f); zeros(n_f,n_f+n_fl)];
JJ = [JJ_fire; zeros(n_f,n_con)];
KK = [KK_fire; zeros(n_f,n_con)];
LL = [LL_firebig, zeros(n_fl,n_senti); zeros(n_f,n_exo+n_senti)];
MM = [MM_firebig, zeros(n_fl,n_senti); zeros(n_f,n_exo+n_s_exo) choose_fcasts_senti];

%Sentiments block law of motion
%NN_senti = zeros(n_senti);
NN_senti = senti_autocorr*eye(n_senti);

%Actual law of motion
NN_alm = zeros(n_exo+n_senti); NN_alm(1:n_exo,1:n_exo) = NN_firebig;
NN_alm(1+n_exo:end,1+n_exo:end) = NN_senti; %sentiments evolve too

%Perceived law of motion (model-specific)
if ~exist('NN_misp','var')
    NN_misp = NN_firebig; %NN_misp is agents' misperception of the NN matrix.  If not specified, set to FIRE.
end
NN_plm = zeros(n_exo+n_senti); NN_plm(1:n_exo,1:n_exo) = NN_misp;
NN_plm(1+n_exo:end,1+n_exo:end) = NN_senti;
NN_plm(1:n_exo,1+n_exo:end)= NN_state_senti;

%solve!
NN = NN_plm;


%Which method should be used: Uhlig (0) or GENSYS (1) or both (2)?
if ~exist('BEET_method','var')
    BEET_method = 0; %default: set it to zero (Uhlig)
end

if BEET_method ~= 1  %calls the Uhlig toolkit
    warnings = [];
    options;
    solve; 
end
if BEET_method == 1  ||  BEET_method == 2  %calls GENSYS
    if ~exist('AA_fire','var')
        di('To convert to GENSYS form, the Uhlig form must be specified without a "y" endogenous variable, i.e. there should be no A, B, C, or D matrix.')
    end
    if ~exist('senti_endovars','var')
        di('Sentiment shocks are not yet compatable with the GENSYS solution.');
    end
    if ~exist('senti_exovars','var')
        di('Sentiment shocks are not yet compatable with the GENSYS solution.');
    end

    %To make consistent, need to add a variable for the behavioral expectation,
    %then additional variables for the current and past rational expectations
    BE_g0 = [-GG -FF; eye(size(GG)) zeros(size(FF))];
    BE_g1 = [HH zeros(size(HH)); zeros(size(HH)) eye(size(HH))];
    BE_c = zeros(2*size(HH,1),1);
    BE_psi = [LL*NN_plm + MM; zeros(size(LL*NN_plm + MM))];
    BE_pi = [zeros(size(HH)); eye(size(HH))];

    [G1,C,impact,fmat,fwt,ywt,gev,eu,loose] = gensys(BE_g0,BE_g1,BE_c,BE_psi,BE_pi);
    
    %Map to Uhlig parameters:
    PP_gensys = G1;
    forwardmat = 0;
    for jj=1:100
        forwardmat = ywt*fmat^(jj-1)*fwt*NN_plm^jj + forwardmat;
    end
    forwardmat=real(forwardmat);
    QQ_gensys = impact+forwardmat;
end
if BEET_method == 1 %if you solved only with GENSYS, save implied Uhlig matrices for post-processing
    PP=PP_gensys;
    QQ=QQ_gensys;
end


%I return NN to match the *actual* law of motion 
%(for other post-processing functions)
NN = NN_alm;

%If you asked by choosing fcast_hors:
%After solving, construct term structure of additional cumulative forecasts
if exist('fcast_hors','var')
    if ~isempty(fcast_hors)
    BEET_foreterm;
    end
end
