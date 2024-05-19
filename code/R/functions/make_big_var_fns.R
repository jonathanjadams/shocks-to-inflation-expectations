##########################################################################################
#
# make_big_var_fns.R
#
# Code to define the functions that make the VARs for the "Inflation Sentiments" project
# Philip Barrett, Washington DC
# First version: 25jan2022
#
##########################################################################################

library(tidyverse)
library(BigVAR)
library(expm)  
library(magrittr)
select <- dplyr::select
# Otherwise picks up the MASS usage

make.big.var <- function( df, value.name, var.name, fcast, inf, y, lag.max=24,  m.Y=NA, ... ){
  # Makes a VAR and returns the reduced form coefficients.  Lags can be AIC, HQ, SC, or FPE
  
  if( is.na(m.Y)){
    m.Y <- df %>%
      filter( !!as.symbol(var.name) %in% c( fcast, inf, y ) ) %>%
      select(date, !!var.name, !!value.name) %>%
      spread( variable, !!value.name ) %>%
      select( fcast, inf, y ) %>%
      as.matrix
    # The matrix of data for the VAR
  }
  n.vars <- ncol(m.Y)
    # The number of variables
  y.means <- m.Y %>% colMeans()
    # Should be v close to zero if de-seasonalizing correctly
  var.names <- m.Y %>% colnames()
    # Names of variables
  
  mod <- constructModel( m.Y,p=lag.max, intercept=FALSE, ... )
    # The BigVAR model
  res <- cv.BigVAR(mod)
    # The result
  m.B.1 <- res@betaPred[,-1] %>% set_rownames(var.names) %>%
    set_colnames( paste0( rep( var.names, lag.max), '.l', rep( 1:lag.max, each=n.vars) ) )
    # The matrix of VAR coefficients
  if( lag.max==1 ){
    m.B <- m.B.1
  }else{
    lagmat <- cbind( diag(n.vars*(lag.max-1)), matrix( 0, n.vars*(lag.max-1), n.vars ) )
    rownames(lagmat) <- colnames(m.B.1)[1:(n.vars*(lag.max-1))]
    m.B <- rbind( m.B.1, lagmat )
  }
  m.Sigma <- (res@resids) %>% set_colnames(var.names) %>% var()
    # The reisidual variance-covariance matrix
  
  return( list( B=m.B, Sigma=m.Sigma, n.vars=n.vars, lags=lag.max, mu=y.means, data=m.Y, bigvar=res ) )
}