%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%           Behavioral Expectations Equilibrium Toolkit
%             Jonathan J. Adams (jonathanjadams.com)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BEET_foreterm: this program calculates the term structure of forecasts
% for a model solved with BEET_solve.m

% Version 0.1 (2023/10/16)

% inputs: 
% - Solution output from BEET_solve.m
% - term length term_H
% - forecast variables fcast_vars and additional cumulative horizons fcast_hors
% - CURRENTLY DOES NOT SUPPORT MULTIPLE EXTENDED FORECASTS OF SAME TYPE FOR SAME DIMENSION


% Set defaults:
if ~exist('term_H','var') 
    %term_H = 20;
    term_H = max(fcast_hors(:,2));
end


%For each horizon hh, construct a forecast of x of form:
% F(x_{t+hh}) = PPf_hh x_t + QQf_hh z_t
%Recursively, they satisfy:
% F(x_{t+hh+1}) = PP*F(x_{t+hh}) + QQ*F(z_{t+hh+1})
% where F(z_{t+hh+1}) = N_plm*F(z_{t+hh})
%which implies
% PPf_{hh+1} = PP*PPf_hh    QQf_{hh+1} = PP*QQf_hh + QQ*N_plm^(hh+1)  

%Specifically, instead of forecasting E[x_{t+hh}] directly, we will forecast 
% the non-rational forecasts F(x_{t+hh})=E[f_{t+hh-1}] (to respect sentiments)
% ... by extracting the forecasts from the x vector instead of the realizations


PPf = zeros(n_fl+n_f,n_fl+n_f,term_H);
QQf = zeros(n_fl+n_f,n_exo+n_senti,term_H);
%PPf(:,:,1) = PP; QQf(:,:,1) = QQ*NN_plm; %from when we forecasted realizations (and below QQ*NN_plm^(hh-1) would have a ^hh instead)
PPf(:,:,1) = eye(n_fl+n_f);
for hh = 2:term_H
    PPf(:,:,hh) = PP*PPf(:,:,hh-1);
    QQf(:,:,hh) = PP*QQf(:,:,hh-1) + QQ*NN_plm^(hh-1);    
end
%This is a vector of E_t[x_t], E_t[x_{t+1}], E_t[x_{t+2}], ...

%identify indices for additional extended forecasts:
fcast_hors_ext=fcast_hors(fcast_hors(:,3)==0,:);    
[common_entries, indices_fcast_hors_ext, indices_fcast_vars_ext] = intersect(fcast_hors_ext,fcast_vars,'stable');
%extended coefficients
    PPef = zeros(length(indices_fcast_hors_ext),n_fl+n_f);
    QQef = zeros(length(indices_fcast_hors_ext),n_exo+n_senti);
    for HH = 1:length(indices_fcast_hors_ext)
        horizon_HH = fcast_hors_ext(indices_fcast_hors_ext(HH),2);
        PPef(indices_fcast_hors_ext(HH),:) = PPf(n_fl+indices_fcast_vars_ext(HH),:,horizon_HH);
        QQef(indices_fcast_hors_ext(HH),:) = QQf(n_fl+indices_fcast_vars_ext(HH),:,horizon_HH);    
    end  

% calculate additional cumulative forecasts:
fcast_hors_cumul=fcast_hors(fcast_hors(:,3)==1,:);    
[common_entries, indices_fcast_hors_cumul, indices_fcast_vars_cumul] = intersect(fcast_hors_cumul,fcast_vars,'stable');
    
    %cumulative coefficients
    PPcf = zeros(length(indices_fcast_hors_cumul),n_fl+n_f);
    QQcf = zeros(length(indices_fcast_hors_cumul),n_exo+n_senti);
    for HH = 1:length(indices_fcast_hors_cumul)
        horizon_HH = fcast_hors_cumul(indices_fcast_hors_cumul(HH),2);
        PPcf(indices_fcast_hors_cumul(HH),:) = sum(PPf(n_fl+indices_fcast_vars_cumul(HH),:,1:horizon_HH),3);
        QQcf(indices_fcast_hors_cumul(HH),:) = sum(QQf(n_fl+indices_fcast_vars_cumul(HH),:,1:horizon_HH),3);    
    end
%end

%now combine cumulative and extended forecast coefficient matrices into "additional forecast"
%coefficient matrices PPaf and QQaf

PPaf = zeros(size(fcast_hors,1),n_fl+n_f);
QQaf = zeros(size(fcast_hors,1),n_exo+n_senti);
PPaf(fcast_hors(:,3)==0,:) = PPef;
QQaf(fcast_hors(:,3)==0,:) = QQef;
PPaf(fcast_hors(:,3)==1,:) = PPcf;
QQaf(fcast_hors(:,3)==1,:) = QQcf;

