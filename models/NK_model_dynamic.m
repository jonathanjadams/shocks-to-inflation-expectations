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



%% Try to make an AS-AD graph
beta = .99;
kappa = .2;
phi_pi = 1.5; phi_y = .0125; gamma = 1; phi_pi_future = .5;

yvecmin = -1;
yvecmax = 1;

yvec = [yvecmin:.01:yvecmax; yvecmin:.01:yvecmax]';
zvec = [zeros(size(yvec,1),1) ones(size(yvec,1),1)];

%aggregate supply pre/post sentiment
ass_vec = beta*zvec + kappa*yvec;

%aggregate demand pre/post sentiment
add_vec = 1/phi_pi*zvec - (phi_y+gamma)/phi_pi*yvec;


pimin = min(min(min(add_vec)),min(min(ass_vec)));

pimax = max(max(max(add_vec)),max(max(ass_vec)));

close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,1),add_vec(end,1),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_preshock.png')
hold off

grayshade = .75;
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,2),add_vec(:,2),'LineWidth',2,'Color','b','Linestyle','--')
plot(yvec(:,2),ass_vec(:,2),'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,2),add_vec(end,2),'AD','FontSize',14)
text(yvec(end,2),ass_vec(end,2),'AS','FontSize',14)
annotation('arrow',[.52 .39],[.4 .7],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_postshock.png')
hold off

%Now one without the policy response!

close all
hold on
plot([0 0],[pimin pimax],'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(0,pimin,'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_nofed_preshock.png')
hold off

grayshade = .75;
zetafudge = .3;

close all
hold on
plot([0 0],[pimin pimax],'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot([zetafudge zetafudge],[pimin pimax],'LineWidth',2,'Color','b','Linestyle','--')
plot(yvec(:,2),ass_vec(:,2),'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(zetafudge,pimin,'AD','FontSize',14)
text(yvec(end,2),ass_vec(end,2),'AS','FontSize',14)
annotation('arrow',[.52 .61],[.4 .75],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_nofed_postshock.png')
hold off

add_vec_nofed = 1/phi_pi*zvec - (phi_y+gamma)/phi_pi*yvec;

% Now going to plot the effects of different folks receiving sentiments

%agg demand for housheold only sentiment is add_vec(:,2)
%agg supply for firm only sentiment is ass_vec(:,2), but I am going to cut
%the change in half to make the grpah nicer:
ass_firmsenti_vec = .5*ass_vec(:,1) + .5*ass_vec(:,2);
%aggregate demand if only fed observes sentiment
add_fedsenti_vec = - phi_pi_future  - (phi_y+gamma)/phi_pi*yvec(:,2);

%Firms only receive sentiment:
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,2),ass_firmsenti_vec,'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,1),add_vec(end,1),'AD','FontSize',14)
text(yvec(end,2),ass_firmsenti_vec(end),'AS','FontSize',14)
annotation('arrow',[.55 .40],[.4 .53],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_firm_senti.png')
hold off

close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
plot(yvec(:,2),add_vec(:,2),'LineWidth',2,'Color','b','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,2),add_vec(end,2),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
annotation('arrow',[.52 .72],[.41 .45],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_hh_senti.png')
hold off

%gotta find that x-intercept for the label
add_fedsenti_vec
[uhh xinterceptdex] = min(abs(add_fedsenti_vec-pimin));

close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
plot(yvec(:,2),add_fedsenti_vec,'LineWidth',2,'Color','b','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(xinterceptdex+7,2),add_fedsenti_vec(xinterceptdex),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
annotation('arrow',[.49 .32],[.39 .36],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_fed_senti.png')
hold off


%% Now without the Taylor principle:

rho = .82;
phi_pi = .2; 

z_scalar = .1;

zvec_ar1 = zvec*z_scalar;


%aggregate supply pre/post sentiment
ass_ar1_vec = (beta*zvec_ar1 + kappa*yvec)/(1-beta*rho);

%aggregate demand pre/post sentiment
add_ar1_vec = (zvec_ar1 + (gamma*(rho - 1) - phi_y)*yvec)/(phi_pi - rho);

pi_ar1_min = min(min(min(add_ar1_vec)),min(min(ass_ar1_vec)));
pi_ar1_max = max(max(max(add_ar1_vec)),max(max(ass_ar1_vec)));

close all
hold on
plot(yvec(:,1),add_ar1_vec(:,1),'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_ar1_vec(:,1),'LineWidth',2,'Color','r')
axis(1.1*[yvecmin yvecmax pi_ar1_min pi_ar1_max])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,1),add_ar1_vec(end,1),'AD','FontSize',14)
text(yvec(end,1),ass_ar1_vec(end,1),'AS','FontSize',14)
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_ar1_preshock.png')
hold off

close all
hold on
plot(yvec(:,1),add_ar1_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_ar1_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,2),add_ar1_vec(:,2),'LineWidth',2,'Color','b','Linestyle','--')
plot(yvec(:,2),ass_ar1_vec(:,2),'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pi_ar1_min pi_ar1_max])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,2),add_ar1_vec(end,2),'AD','FontSize',14)
text(yvec(end,2),ass_ar1_vec(end,2),'AS','FontSize',14)
annotation('arrow',[.5 .25],[.425 .34],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_ar1_postshock.png')
hold off

%% Now with output pessimism
beta = .99;
%kappa = .2;
%stronger kappa in this example:
kappa = 1;

phi_pi = 1.5; phi_y = .0125; gamma = 1; phi_pi_future = .5;
varphi = 4;

yvecmin = -1;
yvecmax = 1;

yvec = [yvecmin:.01:yvecmax; yvecmin:.01:yvecmax]';
zvec = .5*[zeros(size(yvec,1),1) ones(size(yvec,1),1)];

%aggregate supply pre/post sentiment
ass_vec = beta*zvec + kappa*yvec;

%aggregate demand pre/post sentiment
add_vec = (1-varphi*gamma)/phi_pi*zvec - (phi_y+gamma)/phi_pi*yvec;


pimin = min(min(min(add_vec)),min(min(ass_vec)));

pimax = max(max(max(add_vec)),max(max(ass_vec)));

close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color','b')
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color','r')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,1),add_vec(end,1),'AD','FontSize',14)
text(yvec(end,1),ass_vec(end,1),'AS','FontSize',14)
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )
saveas(gcf,'graphs/AD_AS_pessi_preshock.png')
hold off

grayshade = .75;
close all
hold on
plot(yvec(:,1),add_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,1),ass_vec(:,1),'LineWidth',2,'Color',grayshade*[1 1 1])
plot(yvec(:,2),add_vec(:,2),'LineWidth',2,'Color','b','Linestyle','--')
plot(yvec(:,2),ass_vec(:,2),'LineWidth',2,'Color','r','Linestyle','--')
axis(1.1*[yvecmin yvecmax pimin pimax])
set(gca,'XTick',[])
set(gca,'YTick',[])
text(yvec(end,2),add_vec(end,2),'AD','FontSize',14)
text(yvec(end,2),ass_vec(end,2),'AS','FontSize',14)
annotation('arrow',[.5 .23],[.54 .45],'LineWidth',2) %if you mess with parameters, you will have to revisit this one by hand
xlabel('y','FontSize',16,'FontName', 'AvantGarde');
ylabel('\pi','FontSize',16,'FontName', 'AvantGarde');
set(get(gca,'YLabel'),'Rotation',0)
    set(gca, ...
      'Box'         , 'off'     , ...
      'TickDir'     , 'out'     , ...
      'XMinorTick'  , 'on'      , ...
      'LineWidth'   , 1         )

saveas(gcf,'graphs/AD_AS_pessi_postshock.png')
hold off



