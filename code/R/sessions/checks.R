##########################################################################################
#
# checks.R
#
# Code to run checkss for the "Inflation Sentiments" project
# Philip Barrett, Washington DC
# First version: 27jan2022
#
##########################################################################################

message('*** CHECKS ***')

inf.idx <- 2
inf.name <- l.var.coefs$B %>% rownames %>% .[inf.idx]

#### 1. Checks that the phi_h is consistent with the reduced form IRF ####
df.phi.h <- est.phi.h %>%
  as.data.frame( ) %>%
  set_names('phi.h') %>%
  rownames_to_column('shock')
df.irf.h.cumul <- 
  df.irf.rf %>% group_by( shock, outcome ) %>% 
  summarise( value=sum(value[1+1:fcast.horiz]) ) %>% 
  filter(outcome==inf.name) %>% 
  rename(cumul.irf=value) %>%
  select(-outcome)
df.check.rf <- suppressMessages( full_join( df.phi.h, df.irf.h.cumul ) ) %>%
  mutate( err=abs(phi.h-cumul.irf) )
err.irf.rf <- df.check.rf %>%
  pull(err) %>% max
if( err.irf.rf < 1e-08 ){
  message('Estimated phi_h matches reduced form  impulse responses')
}else{
  message('Estimated phi_h does not match reduced form impulse responses, error = ', err.irf.rf)
}

#### 2. Checks that the phi_h is consistent with the structural IRF ####
df.phi.h <- df.irf.struct %>% 
  group_by( shock, outcome ) %>% 
  summarise( value=sum(value[1+1:fcast.horiz]) ) %>% 
  filter(outcome==inf.name) %>% 
  mutate(outcome='phi.h')
df.fcast.0 <- df.irf.struct %>% 
  filter(outcome==fcast, period==0) %>% 
  select(-period, -outcome.f) %>%
  mutate(outcome='fcast')
df.check <- suppressMessages( full_join( df.phi.h, df.fcast.0 ) ) %>%
  spread( outcome, value ) %>%
  mutate( err=abs(phi.h-fcast) )
write_csv( df.check, path='graphs/err_check.csv')
err.irf <- df.check %>%
  filter( shock != 'Non-fundamental') %>%
  pull(err) %>%
  max
if( err.irf < 1e-08 ){
  message('Forecast matches future inflation response to fundamental shocks in the structural VAR')
}else{
  message('Forecast does not match future inflation response to fundamental shocks in the structural VAR, error = ', err.irf)
}

#### 3. Check the structural decomp is accurate ###

## 3.1 Numerical error on the anc operater
err.T.anc <- max( abs( anc.T(est.A$anc, l.var.coefs$Sigma, est.phi.h$phi.h ) - est.A$anc ) )
if( err.T.anc < 1e-08 ){
  message('anc fixed point solved ok')
}else{
  message('anc fixed point failer, error = ', err.T.anc )
}

## 3.2 Sigma check
err.sigma <- max( abs( l.var.coefs$Sigma - est.A$A %*% t(est.A$A) ) )
if( err.sigma < 1e-08 ){
  message('Sigma decomp ok')
}else{
  message('Sigma decomp failed, error = ', err.sigma )
}

#### 4. Check the variance decomposition ratio ####
err.var.decomp <- max(abs(sapply( est.var.decomp$decomp.ratio, rowSums ) - 1))
if( err.var.decomp < 1e-08 ){
  message('Variance decomp ok')
}else{
  message('Variance decomp failed, error = ', err.var.decomp )
}

