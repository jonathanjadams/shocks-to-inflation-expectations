##########################################################################################
#
# robust_roll.R
#
# Code to do rolling-window robustness checks for the "Inflation Sentiments" project
# Philip Barrett, Washington DC
# First version: 12mar2022
#
##########################################################################################

min.length <- 120
  # Minimum number of data points for estimation
v.dates <- df$date %>% unique %>% sort()
n.dates <- v.dates %>% length
append <- FALSE # TRUE
  # Whether to append to the pre-existing data

if(append){
    load(file=paste0( 'data/', fcast, '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_roll.rdata' ))
}

message('*** Robustness check: rolling end window ***')
i.date.init <- if(append) max( min.length, which( v.dates==max(df.robust.end$final.date) ) + 1 ) else min.length
for( i.date in i.date.init:n.dates){
  
  this.date <- v.dates[i.date]
  message('   End date ', this.date )
  this.df <- df %>% filter( date <= this.date )
  this.l.var.coefs <- make.var( this.df, value.name='value.deseas', 
                                var.name='variable', fcast=fcast, 
                                inf=x, y=y, lags=l.var.coefs$lags )
  this.est.phi.h <- sapply( 1:n.fcast,
                       function(i) make.phi.h( this.l.var.coefs, horiz = fcast.horiz, 
                                               inf.idx = n.fcast + i, cumul=fcast.cumul[i] )$phi.h ) %>%
    set_colnames( fcast ) %>% t
  this.est.A <- anc.analytic( this.l.var.coefs$Sigma, this.est.phi.h )
  # this.est.phi.h <- make.phi.h( this.l.var.coefs, horiz = fcast.horiz )
  # this.est.A <- anc.nlslv( this.l.var.coefs$Sigma, this.est.phi.h )
  this.est.bootstrap <- suppressMessages( 
    make.bootstrap( this.l.var.coefs, this.est.A$A, 
                    fcast.horiz=fcast.horiz, inf.name=x, n.pds = fcast.horiz, 
                    do.var.decomp = FALSE, print.iter=FALSE ) )
  this.df.struct <- this.est.bootstrap$structural %>%
    filter(period==0, str_detect(shock, paste0('Sentiment #') ) ) %>%
    select(-period) %>%
    ungroup() %>%
    mutate( final.date=this.date) %>%
    select( outcome, final.date, everything(), -shock )
  df.robust.end <- if(i.date==min.length) this.df.struct else suppressMessages( full_join(df.robust.end, this.df.struct) )
}
df.robust.end <- df.robust.end %>% arrange(outcome, final.date)

save( df.robust.end, file=paste0( 'model_solutions/', this.case.name, '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_roll.rdata' ) )
