##########################################################################################
#
# master.R
#
# Master code to calculate the results using multi-dimensional sentiments
# Philip Barrett, Washington DC
# First version: 21oct2023
#
##########################################################################################

#### 0. Set up global options ####

## 0.1 Housekeeping ##
rm(list=ls())
library(tidyverse)
root.dir <- 'C:\\Users\\pbarrett\\OneDrive - International Monetary Fund (PRD)\\inflation_sentiments\\replication_red'
  ### THE LOCATION OF THE REPLICATION FOLDER: NEEDS CHANGING FOR YOUR LOCAL SYSTEM ###
setwd(root.dir)
xl.in.dta <- 'data/Baseline Time Series/inflation_sentiments'
#### TO ADD: CLEAR THE OLD GRAPHS AND TABLES ####

## 0.2 Define functions ####
source('code/R/functions/make_var_fns.R')
source('code/R/functions/make_irf_fns.R')
source('code/R/functions/make_lp_fns.R')
source('code/R/functions/make_non_RE_var_fns.R')

## 0.3 Controls ##
make.data <- TRUE # FALSE #                  # Remake the data from scratch?
data.file <- 'data/baseline_time_series_update.rdta'     # Data
out_prefix <- 'replication/'               # Where to store the outputs
n.boot <- 1000 # 100 # 500 # Set a small number for testing
l.cases <- list( 
  michigan_cpi=list(freq='m', fcast='michigan.fcast',
                    x='inf.cpi', fcast.cumul=c(TRUE),
                    y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                    init.yr=NA, robust=TRUE,
                    do.lp=TRUE, do.non.re=TRUE, non.re.ci=TRUE),
  michigan_cpi_1st=list(freq='m', fcast='michigan.fcast',
                    x='inf.cpi.first', fcast.cumul=c(TRUE),
                    y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                    init.yr=NA, robust=FALSE,
                    do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_2010=list(freq='m', fcast='michigan.fcast',
                        x='inf.cpi', fcast.cumul=c(TRUE),
                        y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                        init.yr=NA, robust=FALSE, final.yr=2010,
                        do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_2015=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, final.yr=2015,
                         do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_trend=list(freq='m', fcast='michigan.fcast.trend',
                        x='inf.cpi', fcast.cumul=c(TRUE),
                        y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                        init.yr=NA, robust=FALSE,
                        do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_pi_cpi=list(freq='m', fcast=c( 'log.real.pi.fcast', 'michigan.fcast'),
                       x=c('log.real.pi', 'inf.cpi'), fcast.cumul=c(FALSE, TRUE),
                       y=c( 'fed.funds.rate', 'unemp', 'inf.commod'),
                       init.yr=NA, robust=FALSE, do.lp=FALSE,
                       do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE ),
  spf_gdp_pgdp=list(freq='q', fcast=c('spf.fcast.log.gdp', 'spf.fcast'),
                    x=c('log.gdp','inf.gdp'), fcast.cumul=c(FALSE, TRUE),
                    y=c('fed.funds.rate', 'log.inv', 'log.cons'),
                    init.yr=1982, robust=FALSE, do.lp=FALSE,
                    do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE ),
  spf_unemp_gdp_pgdp=list(freq='q', fcast=c('spf.fcast.unemp', 'spf.fcast.log.gdp', 'spf.fcast'),
                    x=c('unemp','log.gdp','inf.gdp'), fcast.cumul=c(FALSE, FALSE, TRUE),
                    y=c('fed.funds.rate', 'log.inv', 'log.cons'),
                    init.yr=1982, final.yr=2021, robust=FALSE, do.lp=FALSE,
                    do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE ),
  spf_tbill_gdp_pgdp=list(freq='q', fcast=c('spf.fcast.irate', 'spf.fcast.log.gdp', 'spf.fcast'),
                    x=c('tbill','log.gdp','inf.gdp'), fcast.cumul=c(FALSE, FALSE, TRUE),
                    y=c('unemp', 'log.inv', 'log.cons'),
                    init.yr=1982, final.yr=2021, robust=FALSE, do.lp=FALSE,
                    do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE ),
  cleveland_cpi=list(freq='m', fcast='cleveland.fcast',
                     x='inf.cpi', fcast.cumul=c(TRUE),
                     y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                     init.yr=NA, robust=FALSE,
                     do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  fed_pgdp= list(freq='q', fcast='fed.fcast',
                 x='inf.gdp', fcast.cumul=c(TRUE),
                 y=c('fed.funds.rate', 'log.gdp', 'log.inv', 'log.cons'),
                 init.yr=1982, final.yr=2016, robust=FALSE,
                 do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE ),
  spf_pgdp=list(freq='q', fcast='spf.fcast',
                x='inf.gdp', fcast.cumul=c(TRUE),
                y=c('fed.funds.rate', 'log.gdp', 'log.inv', 'log.cons'),
                init.yr=1982, robust=FALSE,
                do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  spf_gdp=list(freq='q', fcast='spf.fcast.log.gdp',
               x='log.gdp', fcast.cumul=c(TRUE),
               y=c('fed.funds.rate', 'inf.gdp', 'log.inv', 'log.cons'),
               init.yr=1982, robust=FALSE,
               do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_oil_inf=list(freq='m', fcast='michigan.fcast',
                    x='inf.cpi', fcast.cumul=c(TRUE),
                    y=c('inf.oil', 'fed.funds.rate', 'unemp', 'log.ip' ),
                    init.yr=NA, robust=FALSE, order.first='inf.oil',
                    do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_oil_inf_2=list(freq='m', fcast='michigan.fcast',
                        x='inf.cpi', fcast.cumul=c(TRUE),
                        y=c('inf.oil', 'fed.funds.rate', 'unemp', 'log.ip', 'inf.commod' ),
                        init.yr=NA, robust=FALSE, order.first='inf.oil',
                        do.lp=FALSE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_1lag=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, lags=1,
                         do.lp=TRUE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_2lag=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, lags=2,
                         do.lp=TRUE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_4lag=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, lags=4,
                         do.lp=TRUE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_6lag=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, lags=6,
                         do.lp=TRUE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_8lag=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, lags=8,
                         do.lp=TRUE, do.non.re=FALSE, non.re.ci=FALSE),
  michigan_cpi_12lag=list(freq='m', fcast='michigan.fcast',
                         x='inf.cpi', fcast.cumul=c(TRUE),
                         y=c( 'inf.commod', 'fed.funds.rate', 'unemp', 'log.ip' ),
                         init.yr=NA, robust=FALSE, lags=12,
                         do.lp=TRUE, do.non.re=FALSE, non.re.ci=FALSE)
)

## 0.4 Settings for the non-RE functions ##
theta.bord <- .55 ; theta.bord.indiv <- - 0.15
  # Convert to the DE thetas
theta.christiano <- .83 ; theta.gelain <- .91
l.non.re.fns <- c('Delayed Observation'=make.phi.h.DO, 
                  'Adaptive expectations, Gelain et al. 2019'=make.phi.h.AE.gelain,
                  'Partial Observation, FFR'=make.phi.h.PO.fed.funds,
                  'Partial Observation, FFR + Commodity CPI'=make.phi.h.PO.cdty,
                  'Diagnostic Expectations, over-reaction'=make.phi.h.DE.bord,
                  'Diagnostic Expectations, under-reaction'=make.phi.h.DE.bord.indiv #,
)
v.non.re.idx.show <- 1:length(l.non.re.fns)
l.non.re.show <- l.non.re.fns[v.non.re.idx.show] %>% names()
  # The non-RE functions

## 0.5 Derived settings ##
n.cases <- l.cases %>% length()           # The number of cases 
make.dir.if.needed <- function(dir.path) if(!file.exists(dir.path)) dir.create(dir.path,recursive = TRUE)
for( this.vars in c( 'compare', names(l.cases)) ) make.dir.if.needed(paste0('graphs/',out_prefix, this.vars))
  # Make the save files if needed

########## Main Computational loop
for(i.case in 1:n.cases ){
  
  #### 1. Set up ####
  
  ## 1.1 Extract controls
  freq <- l.cases[[i.case]]$freq
  fcast <- l.cases[[i.case]]$fcast
  fcast.cumul <- l.cases[[i.case]]$fcast.cumul
  init.yr <- l.cases[[i.case]]$init.yr
  # fcast.var.name <- l.cases[[i.case]]$fcast.var.name
  final.yr <- if( is.null(l.cases[[i.case]]$final.yr) ) NA else l.cases[[i.case]]$final.yr
  x <- l.cases[[i.case]]$x
  y <- l.cases[[i.case]]$y
  do.lp <- l.cases[[i.case]]$do.lp
  non.re.ci <- l.cases[[i.case]]$non.re.ci
  do.non.re <- l.cases[[i.case]]$do.non.re
  robust <- if( is.null(l.cases[[i.case]]$robust) ) FALSE else l.cases[[i.case]]$robust
  irf.pds <- if(freq=='m') 36 else 16
  irf.plot.pds <- irf.pds - if(freq=='m') 12 else 4
  n.fcast <- fcast %>% length()
  n.vars <- 2*n.fcast + length(y)
  # if( freq=='q') l.non.re.fns <- l.non.re.fns[ !grepl( 'Commodity', names(l.non.re.fns) ) ]
  n.non.re <- l.non.re.fns %>% length()
  this.case.name <- names(l.cases)[i.case]
  order.first <- if( is.null(l.cases[[i.case]]$order.first) ) 'none' else l.cases[[i.case]]$order.first
  lags <- if(is.null(l.cases[[i.case]]$lags)) NA else l.cases[[i.case]]$lags
  
  ## 1.2 Monitoring ##
  message( '\n\n******** CASE # ', i.case, ' ********')
  message( 'Forecast series = ', paste0( fcast, collapse=', '),
           ', Initial year = ', init.yr, '\n\n' )
  
  
  #### 2. Read and process the data ####
  if( make.data){
    source('code/R/sessions/make_clean_data_US.R')
  }else{
    load(data.file)  
  }
  df <- if(freq=='m') df.m else df.q
  init.yr <- if(is.na(init.yr)) df$date %>% min %>% year else init.yr
  final.yr <- if(is.na(final.yr)) df$date %>% max %>% year else final.yr
  df <- df %>% filter( year( date ) >= init.yr, year( date ) <= final.yr ) 
  
  #### 3. Estimate the reduced forms ####
  # stop()
  if(!is.na(lags)){
    l.var.coefs <- make.var( df, value.name='value.deseas', 
                             var.name='variable', fcast=fcast, 
                             inf=x, y=y, lags=lags )
  }else{
    l.var.coefs <- make.var( df, value.name='value.deseas', 
                           var.name='variable', fcast=fcast, 
                           inf=x, y=y, lag.max=8 )
  }
    
  #### 4. Compute the structural decomposition ####
  fcast.horiz <- if(freq=='m') 12 else 4
  est.phi.h <- sapply( 1:n.fcast,
     function(i) make.phi.h( l.var.coefs, horiz = fcast.horiz, 
       inf.idx = n.fcast + i, cumul=fcast.cumul[i] )$phi.h ) %>%
    set_colnames( fcast ) %>% t
  reorder.to.idx <- if(order.first=='none') 0 else n.fcast * 2 + which(y==order.first)
  est.A <- anc.analytic( l.var.coefs$Sigma, est.phi.h ) %>%
    A.reorder(., l.var.coefs$Sigma, n.fcast, reorder.to.idx )
    # Include the reordering
  # stop()
  est.var.decomp <- var.decomp( est.A$A, l.var.coefs, n.pds = irf.pds, n.fcast=n.fcast )
  if(do.non.re){
    l.non.re.phi <- lapply( l.non.re.fns, function(fn) fn(l.var.coefs, horiz = fcast.horiz) )
    l.est.A.non.re <- lapply( l.non.re.phi, function(x) anc.analytic( l.var.coefs$Sigma, x$phi.h ) )
  }
  if(do.lp) est.lp <- make.lp.irf( df, est.A, l.var.coefs, inf.name=x, this.fcast=fcast, irf.pds=irf.pds )
  
  #### 5. Make the IRFs ####
  # Bootstrap the residuals
  est.bootstrap <- make.bootstrap( l.var.coefs, est.A$A, fcast.horiz=fcast.horiz,
                                   inf.name=x, n.pds = irf.pds, n.boot = n.boot,
                                   fcast.cumul=fcast.cumul, print.iter = FALSE,
                                   reorder.to.idx=reorder.to.idx )
  if(non.re.ci){
    l.est.bootstrap.non.re <- lapply( 1:n.non.re, function(i){
      message( paste0( '### Bootstrap, ', names(l.non.re.fns)[i], ' ###' ) )
      make.bootstrap( l.var.coefs, l.est.A.non.re[[i]]$A, fcast.horiz=fcast.horiz, inf.name=x,
                      n.pds = irf.pds, n.boot = n.boot, fcast.cumul=fcast.cumul,
                      make.phi.fn = l.non.re.fns[[i]], print.iter = FALSE, do.var.decomp = FALSE,
                      reorder.to.idx=reorder.to.idx )
    } ) %>% set_names( names(l.non.re.fns) )
  }
  
  
  # #### 6 Do the robustness checks ####
  if(robust){
    source('code/R/sessions/robust_selection.R')
    source('code/R/sessions/robust_roll.R')
  }
    # This takes a *really* long time :(
  
  #### 7. Make some charts ####
  figure.location <- paste0( out_prefix, names(l.cases)[[i.case]] )

  ## 7.1 Quick IRFs before all the hard work ##
  df.irf.struct <- make.irf.struct( l.var.coefs, est.A$A, fcast.horiz, x, 
                             irf.pds, fcast, fcast.cumul)
  df.quick.irfs <- lapply( 0:irf.pds, 
         function(i) ((l.var.coefs$B %^% i)[1:n.vars,1:n.vars] %*% est.A$A) %>%
           as.data.frame %>% rownames_to_column('variable') %>%
           gather(shock, value, -variable) %>% mutate(pd=i) ) %>%
    reduce(full_join)
  
  ggplot( df.irf.struct %>% filter( grepl('Sentiment', shock ) ),
          aes(x=period, y=value, color=shock, shape=shock) ) +
    geom_hline(aes(yintercept=0)) +
    geom_line(lwd=1.2) +
    geom_point(size=3) +
    facet_wrap(~outcome, scales='free_y') +
    theme_minimal() + 
    theme(legend.position = 'bottom')
  ggsave(paste0('graphs/', figure.location,'/zz_quick_irfs.pdf'))
  
  ## 7.2 Now make lots more charts ##
  source('code/R/sessions/charts.R')
  
  #### 8. Save the data ####
  save.string <- c( 'est.A', 'l.var.coefs', 'df', 'df.irf.rf', 'df.irf.struct', 'df.var.decomp', 
                    'est.bootstrap', if(non.re.ci) 'l.est.bootstrap.non.re', if(do.lp) 'est.lp' )
  save( list=save.string, file=paste0( 'model_solutions/', this.case.name, '_', init.yr, '_', freq, '_', l.var.coefs$lags, '.rdata' ) )
  
  df.shk.ar <- df.irf.struct %>% 
    filter( grepl( 'Sentiment', shock ), grepl('sentiment', outcome) ) %>% 
    group_by(shock,outcome) %>% 
    summarise( ar.1=ar(na.omit(value),order=1, aic=FALSE)$ar )
  # write_csv( df.shk.ar, file = paste0( 'graphs/', figure.location, '/shk_ar.csv') )
  
  struct.resid <- solve( est.A$A, l.var.coefs$var %>% resid() %>% t ) %>% t
  df.struct.resid <- data.frame( date=(df %>% select(-value.deseas) %>% 
                                         spread( variable, value ) %>% 
                                         pull(date))[-(1:l.var.coefs$lags)],
                                 struct.resid )
  # write_csv( df.struct.resid, file=paste0( this.case.name, '_resid.csv' ) )
}


#### 9. Tidying up ####
source('code/R/sessions/comparison_charts.R')
source('code/R/sessions/simulation_nD.R')
source('code/R/sessions/narrative_compare.R')
source('code/R/sessions/renaming.R')
