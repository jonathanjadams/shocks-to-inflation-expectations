% This code solves the dynamic New Keynesian model with shocks to expectations 
% appearing in Adams and Barrett (2024) "Shocks to Inflation Expectations"
% Section 6: "Dynamic Model and Monte Carlo Validation"
% 
% the model is solved using BEET, a wrapper for the Uhlig toolkit, which
% was developed to solve DSGE models without rational expectations
%
% Dependencies: BEET toolkit, Uhlig toolkit (add to path)
%
% Output: 
% Simulated datasets: simdata.csv simshocks.csv
% Model IRFs: simirfs.csv, simirfs_more.csv 


%Parameters
timescale = 1/3; %monthly = 1/3, quarterly = 1
siggma = 1;
varphi = 1; 
theta=3/4;
rho_x = 0.12^timescale;
rho_z  = 0.18^timescale;
rho_zeta = (.63)^3^timescale;  %monthly autocorrelation is .63  

rho_a  = 0.95^timescale;
phi_pi = 1.5;
phi_y  = 0.125;
betta  = 0.99^timescale;
eta  = 3.77; 
alppha=1/3;
epsilon=6;
sigma_x = .14; %Interest rate shock s.d.
sigma_z = .14; %Discount factor shock s.d.
sigma_a = (.45^2*timescale)^.5;  %This parameter at .45 matches Gali's quarterly calibration
sigma_nu = sigma_a;  %Half of the productivity variance is forecastable as news
sigma_zeta = .145; 

%Calculate news/surprise variances:
var_news = sigma_a^4 / (sigma_a^2 + sigma_nu^2);
var_surprise = sigma_a^2 - var_news;

%Composite parameters
Omega=(1-alppha)/(1-alppha+alppha*epsilon);        %defined on Gali page 60
psi_n_ya=(1+varphi)/(siggma*(1-alppha)+varphi+alppha);   %defined on Gali page 62
lambda=(1-theta)*(1-betta*theta)/theta*Omega;      %defined on Gali page 61
kappa=lambda*(siggma+(varphi+alppha)/(1-alppha));     %defined on Gali page 63


%"state" vector is [pi, y]';
pi_index = 1; y_index = 2; %entries in "x" vector (forward-looking variables)
%"jump" vector is i;
i_index = 1; %entries in "y" vector (contemp. variables)
%variable vector is [pi y i]';
%exog state vector is [nu z a a_news]';
x_index = 1; z_index = 2; a_index = 3; anews_index = 4;
n_exo_shocks = 4;

%first set of matrices for non-expectational eqn (taylor rule)
AA_fire = [-phi_pi, -phi_y]; 
BB_fire = 0*AA_fire; 
CC_fire = 1; %this matrix needs to be square!
DD_fire = zeros(1,n_exo_shocks); DD_fire(x_index)=-1;

%second set for expectational eqn (ordered Euler, NKPC):
%FF = zeros(2,0);
%GG = FF; HH = FF;
FF_fire = [-1 -siggma; -betta 0]; 
GG_fire = [0 siggma; 1 -kappa];
HH_fire = 0*GG_fire; 
JJ_fire = [0;0]; 
KK_fire = [1;0]; 
LL_fire = zeros(2,n_exo_shocks); 
MM_fire = zeros(2,n_exo_shocks); MM_fire(1,z_index) = 1; MM_fire(2,a_index) = kappa*psi_n_ya; %[0 0 1 0; 0 kappa*psi_n_ya 0 0]; 
%Base law of motion
NN_fire = zeros(n_exo_shocks); NN_fire(x_index,x_index)=rho_x; NN_fire(z_index,z_index)=rho_z; 
    NN_fire(a_index,a_index)=rho_a; NN_fire(a_index,anews_index)=1; %yesterday's productivity news

%Vector of shock standard deviations:
sigma_vec = [sigma_x sigma_z sqrt(var_surprise) sqrt(var_news) sigma_zeta sigma_zeta sigma_zeta];
%Implied variance matrix:
Sigma = diag(sigma_vec.^2);

%which forecasted endogenous variables have sentiments?? (must also have a forecast)
senti_endovars = [pi_index y_index]; 

%add additional cumulative forecast horizons to the model (horizon 1 is assumed):
%fcast_hors is a tuple: [which forecast dimension, which horizon, and cumulative or not (1 or 0)]
fcast_hors = [pi_index 12 1; y_index 12 0];

%which forecasted exogenous variables have sentiments?? 
senti_exovars = a_index;

senti_autocorr = rho_zeta;  %autocorrelation for sentiment shocks

ztitles=cell(1,n_exo_shocks); ztitles(x_index)={'x'}; ztitles(z_index)={'z'}; ztitles(a_index)={'a'}; ztitles(anews_index)={'a news'};
xtitles={'pi','y'};
ytitles={'i'};


%solve:
BEET_solve;

irf_T = 100; %how long will the irfs be?
BEET_irfs; %calculate IRFs (will generate some plots - these are not needed for replication)

BEET_sim; %simulate the economy 

var_decomp
display(strcat('inflation sentiment share of income variance: ',num2str(var_tab(2,6,21)/sum(var_tab(2,:,21)))))
%target share estimated from the VAR:  11%

%% save results for Monte Carlo analysis in R

fpi_index = 6; fy_index = 7;  
pi_xyindex = (size(CC_fire,1)+pi_index);
y_xyindex = (size(CC_fire,1)+y_index);
i_xyindex = i_index;

irf_SAVE = 1:36; %select a subset of horizons to save
xy_indices_save = [fpi_index fy_index pi_xyindex y_xyindex i_xyindex]; %select a subset of variables to save

simdata = [xy_sim(xy_indices_save,:)'];
eps_zeta_pi_index = n_exo_shocks + length(senti_exovars)+1; %4 exo shocks, 1 exo senti shock, then first of 2 endo senti shocks
simirfs = [xy_irf(xy_indices_save,irf_SAVE,eps_zeta_pi_index)']; %IRFs to the inflation sentiment shock
writematrix(simdata,'simdata.csv') 
writematrix(eps_sim','simshocks.csv') 
writematrix(simirfs,'simirfs.csv') 

xy_indices_more = [4 5]; %one period ahead forecasts
REy12irf = xy_irf(y_xyindex,13:end,eps_zeta_pi_index); %Construct the rational forecast for 12-period-ahead y
REpi12irf = 12*movmean(xy_irf(pi_xyindex,2:end,eps_zeta_pi_index),[0 11]); %Construct the rational forecast for cumulative 12-period-ahead pi %check: REpi12irf(1)- sum(xy_irf(pi_xyindex,2:13,eps_zeta_pi_index))
%xy_irf(xy_indices_more,irf_SAVE,eps_zeta_pi_index)' 1 month ahead forecast irfs
pi_senti_irf = xy_irf(fpi_index,1:(end-1),eps_zeta_pi_index) - REpi12irf; %Construct the inflation sentiment (belief distortion)
moreirfs = [ REpi12irf(irf_SAVE)' REy12irf(irf_SAVE)' pi_senti_irf(irf_SAVE)' z_irf(eps_zeta_pi_index,irf_SAVE,eps_zeta_pi_index)'];
%check: [REy12irf(irf_SAVE)' xy_irf(fy_index,irf_SAVE,eps_zeta_pi_index)']
%check: [REpi12irf(irf_SAVE)' xy_irf(fpi_index,irf_SAVE,eps_zeta_pi_index)']
writematrix(moreirfs,'simirfs_more.csv') 



