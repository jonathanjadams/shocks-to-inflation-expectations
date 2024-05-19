##########################################################################################
#
# robust_selection.R
#
# Code to do model selection via the BigVAR package
# Philip Barrett, Washington DC
# First version: 25jan2022
#
##########################################################################################

#### 0. Set up ####
source("code/R/functions/make_big_var_fns.R")
library(xtable)
library(magrittr)

#### 1. Controls ####
all.covars <- df %>% pull(variable) %>% unique %>% .[ !(. %in% c(x, 'michigan.fcast', 'cleveland.fcast', 'fed.funds.diff', 'unemp.diff') ) ]
small.covars <- c( l.cases[[i.case]]$y, 'log.bank.credit', 'log.emp', 'log.fx.gbr', 'log.fx.jpn', 'g.surp.idx', 
                   'i.3.mo', 'i.1.yr', 'i.2.yr', 'i.5.yr', 'i.30.yr', 'i.10.yr', 'leverage.idx', 'log.will.idx', 
                   'log.m2', 'log.vehicles', 'log.housing.starts', 'log.currency') 
  # 'log.m1', 'log.initial.claims',  'log.p.oil.r',
  # For some reason M1 really messes things up.  And initial claims.  Both have **huuuuge** jumps at the onset of the pandemic.
v.methods <- c('Basic', 'BasicEN', 'HVARC', 'Tapered')
  # The controls for the ML stuff

#### 2. FAVAR #####
keep.vars <- c( x, fcast, 'fed.funds.rate', 'log.ip' )
small.covars.no.y <- small.covars[ !(small.covars %in% keep.vars ) ]
df.wide <- df %>% select(-value) %>% spread( variable, value.deseas )
prc <- df.wide %>% prcomp( as.formula( paste0( '~', paste( small.covars, collapse='+') ) ), data=., scale=TRUE )
  # The PC decomp
prc.data <- predict( prc, newdata=df.wide )
  # The PRC data
df.pc <- df.wide %>% select( date, keep.vars ) %>% cbind( ., prc.data ) %>%
  gather( variable, value.deseas, -date )
  # Added back into the data frame
sd.cumul <- (prc$sdev / sum(prc$sdev)) %>% cumsum()
  # The fraction of the variance explained by each principal component
v.cutoff <- c(.5, .75, .9 )
n.favar <- sapply( v.cutoff, function(x) (sd.cumul < x) %>% sum() )
  # The number of factors below each cutoff
df.favar.irf.struct <- l.var.coefs.favar <- l.est.phi.h.favar <- l.est.A.favar <- list()
  # Initialize the outputs
for( i in 1:length(v.cutoff) ){
  l.var.coefs.favar[[i]] <- make.var( df.pc, value.name='value.deseas', var.name='variable', fcast=fcast, inf=x,
                                      y= c( 'fed.funds.rate', 'log.ip', paste0('PC', 1:n.favar[i])), lag.max=8 ) #, lags = 2 )

  l.est.phi.h.favar[[i]] <- sapply( 1:n.fcast,
                            function(j) make.phi.h( l.var.coefs.favar[[i]], horiz = fcast.horiz,
                                                    inf.idx = n.fcast + j, cumul=fcast.cumul[j] )$phi.h ) %>%
    set_colnames( fcast ) %>% t
  l.est.A.favar[[i]] <- anc.analytic( l.var.coefs.favar[[i]]$Sigma, l.est.phi.h.favar[[i]] )
  # l.est.phi.h.favar[[i]] <- make.phi.h( l.var.coefs.favar[[i]], horiz = fcast.horiz )
  # l.est.A.favar[[i]] <- anc.nlslv( l.var.coefs.favar[[i]]$Sigma, l.est.phi.h.favar[[i]] )

  df.favar.irf.struct[[i]] <- make.irf.struct(l.var.coefs.favar[[i]], l.est.A.favar[[i]]$A,
                                              fcast.horiz=fcast.horiz,
                                              inf.name=x, n.pds = irf.pds, fcast = fcast ) %>%
    mutate( var.method=paste0( n.favar[i], ' component FAVAR' ) )
  other.vars <- df.favar.irf.struct[[i]] %>% pull(outcome) %>% unique %>% .[!(.==fcast)]
  df.favar.irf.struct[[i]] <- df.favar.irf.struct[[i]] %>%
    mutate( outcome.f=factor( outcome, levels=c(fcast,other.vars) ) )
    # The structural IRF
  this.est.bootstrap <- make.bootstrap( l.var.coefs.favar[[i]], l.est.A.favar[[i]]$A, seed = 100,
                                        fcast.horiz=fcast.horiz, inf.name=x, n.pds = fcast.horiz,
                                        n.boot = n.boot, do.var.decomp = FALSE, print.iter=FALSE )
  this.df.struct <- this.est.bootstrap$structural %>%
    filter(period==0, shock=='Sentiment #1') %>%
    select(-period) %>%
    ungroup() %>%
    mutate( var.method=paste0( n.favar[i], ' component FAVAR' ) ) %>%
    select( outcome, var.method, everything(), -shock )
  df.favar.boot <- if(i==1) this.df.struct else suppressMessages( full_join(df.favar.boot, this.df.struct) )
}
df.favar.irf.struct.full <- df.favar.irf.struct %>% reduce(full_join)
df.favar.boot <- df.favar.boot %>%
  mutate( var.method = fct_relevel(var.method, paste0( n.favar, ' component FAVAR' ) ) )

save( l.var.coefs.favar, l.est.A.favar, df.favar.irf.struct.full, df.favar.boot,
      file=paste0( 'model_solutions/', this.case.name, '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_FAVAR.rdata' ) )

#### 3. Machine learning via BigVAR ####
l.big.var.coefs <- big.est.phi.h <- big.est.A <- df.big.irf.struct <- list()
for( this.method in v.methods ){
  l.big.var.coefs[[this.method]] <- make.big.var( df, value.name='value.deseas', var.name='variable', 
                                   fcast=fcast, inf=x, y=small.covars, lag.max=6, 
                                   struct = this.method, gran=c(10000,80), RVAR=FALSE,h=1,cv="Rolling",
                                   MN=FALSE, verbose=TRUE, IC=TRUE )
  big.est.phi.h[[this.method]] <- sapply( 1:n.fcast,
                                    function(j) make.phi.h( l.big.var.coefs[[this.method]], horiz = fcast.horiz, 
                                                            inf.idx = n.fcast + j, cumul=fcast.cumul[j] )$phi.h ) %>%
    set_colnames( fcast ) %>% t
  # big.est.phi.h[[this.method]] <- make.phi.h( l.big.var.coefs[[this.method]], horiz = fcast.horiz )
  big.est.A[[this.method]] <- anc.analytic( l.big.var.coefs[[this.method]]$Sigma, big.est.phi.h[[this.method]] )
  
  df.big.irf.struct[[this.method]] <- 
    make.irf.struct(l.big.var.coefs[[this.method]], big.est.A[[this.method]]$A, 
                    fcast.horiz=fcast.horiz, inf.name=x, n.pds = irf.pds, fcast = fcast ) %>%
    mutate( var.method=this.method )
  other.vars <- df.big.irf.struct[[this.method]] %>% pull(outcome) %>% unique %>% .[!(.==fcast)]
  df.big.irf.struct[[this.method]] <- df.big.irf.struct[[this.method]] %>% mutate( outcome.f=factor( outcome, levels=c(fcast,other.vars) ) )
}
df.big.irf.struct.full <- df.big.irf.struct %>% reduce(full_join)

save( l.big.var.coefs, big.est.A, df.big.irf.struct.full,
      file=paste0( 'model_solutions/', this.case.name, '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_selection.rdata' ) )

m.tab.selection <- 
  cbind( 'Avg.'=c( frac=NA, mean.msfe=l.big.var.coefs[[1]]$bigvar@MeanMSFE, sd.msfe=l.big.var.coefs[[1]]$bigvar@MeanSD ),  
         AIC = c( frac=NA, mean.msfe=l.big.var.coefs[[1]]$bigvar@AICMSFE, sd.msfe=l.big.var.coefs[[1]]$bigvar@AICSD ),
         sapply( l.big.var.coefs, function(x) c( frac=x$bigvar@sparse_count, mean.msfe=x$bigvar@OOSMSFE %>% mean(), sd.msfe=x$bigvar@seoosmsfe ) ) ) %>%
  set_rownames( c('Frac. active coefficients', 'Mean MSFE', 'MSFE st dev'))
xtable(m.tab.selection, caption = 'Model selection by machine learning: Forecast evaluation.', 
       label='t:ML_eval', align=c('l','r','r','r','r','r','r') ) %>%
  print( file='tables/ml_eval.tex', floating=FALSE, hline.after=c(-1,-1,0,3) )
# To add: table output





