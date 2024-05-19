##########################################################################################
#
# make_var_fns.R
#
# Code to define the functions that make the VARs for the "Inflation Sentiments" project
# Philip Barrett, Washington DC
# First version: 25jan2022
#
##########################################################################################

library(tidyverse)
library(vars)
library(expm)   
library(lubridate)
library(magrittr)
select <- dplyr::select
# Otherwise picks up the MASS usage

make.var <- function( df, value.name, var.name, fcast, inf, y,
                      lags=NA, lag.max=24, lag.select='AIC', m.Y=NA ){
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
  # browser()
  if( lags %>% is.na ){
    lagselect <- VARselect( m.Y, lag.max=lag.max )
    lags <- lagselect$selection[paste0(lag.select, '(n)')]
  }
    # Select the lag structure if required
  p <- lags
    # Because using the name "lags" causes errors later.  Weird.
  var.est <- VAR( m.Y, p, type='none', season=NULL, exog=NULL )
    # The estimated reduced-form VAR
  m.B.1 <- var.est$varresult %>% sapply( ., coef) %>% t
    # The matrix of VAR coefficients
  if( p==1 ){
    m.B <- m.B.1
  }else{
    lagmat <- cbind( diag(n.vars*(p-1)), matrix( 0, n.vars*(p-1), n.vars ) )
    rownames(lagmat) <- colnames(m.B.1)[1:(n.vars*(p-1))]
    m.B <- rbind( m.B.1, lagmat )
  }
  # m.Sigma <- var.est %>% summary %>% .$covres
  m.Sigma <- var.est %>% resid() %>% var()
    # The reisidual variance-covariance matrix
  
  return( list( B=m.B, Sigma=m.Sigma, n.vars=n.vars, lags=p, mu=y.means, data=m.Y, var=var.est ) )
}

make.phi.h <- function( l.var, horiz=12, inf.idx=2, cumul=TRUE ){
# Makes the cummulative response for inflation at horizon h.  Assumes ordering
# of shocks is (fcast, inf, others)
  
  n.vars <- l.var$n.vars
    # Number of variables
  phi.k <- if(cumul) lapply( 1:horiz, function(x) l.var$B %^% x ) %>%
    Reduce( '+', . ) else l.var$B %^% horiz
    # The cummulative IRFs for all variables
  out <- list()
  out$phi.h <- phi.k[inf.idx,1:n.vars] ; names(out$phi.h) <- rownames(phi.k)[1:n.vars]
  return( out )
    # Select the inflation response
}

make.var.fcast <- function( m.B, m.Y, n.fcast.pds=1 ){
# Makes a forecast of the VAR, n.pds ahead
  n.vars <- m.Y %>% ncol()
  n.pds <- m.Y %>% nrow()
  n.lags <- (m.B %>% ncol) / n.vars
    # Problem dimensions
  m.fcast <- (( m.B %^% n.fcast.pds ) %>% t()) %>% .[,1:n.vars]
    # The multi-period forecast matrix
  m.out <- matrix( NA, n.pds, n.vars ) %>% set_colnames( colnames(m.Y) )
    # The output matrix
  m.Y.use <- matrix(NA, n.pds, n.vars*n.lags ) %>% set_colnames( rownames(m.B) )
  for( j in 1:n.lags ) m.Y.use[j:n.pds,(j-1)*n.vars+1:n.vars] <- m.Y[1:(n.pds+1-j),]
    # Fill out the data to use as a matrix
  for( i in n.lags:n.pds ){
    m.out[i,] <- m.Y.use[i,] %*% m.fcast
  }
  return(m.out)
}

make.var.fcast.fit <- function( l.var, n.fcast.pds=1, cumul=FALSE ){
# Makes a forecast of the VAR, n.pds ahead
  
  out <- if(cumul){
    lapply( 1:n.fcast.pds, function(i) make.var.fcast(l.var$B, l.var$data, i) ) %>%
      Reduce('+', .)
  }else{
    make.var.fcast(l.var$B, l.var$data, n.fcast.pds )
  }
  return(out)
}

var.decomp <- function( A, l.var, n.pds=20, n.fcast=1 ){
# The variance decomposition
  
  l.var.decomp.ratio <- l.var.decomp <- list()
  n.vars <- l.var$n.vars
    # Number of variables
  p <- l.var$lags
    # The number of lags
  big.sigma <- l.var$Sigma
  B <- l.var$B
    # Extract the VAR elements
  
  A.sq.extended <- lapply( 1:n.vars, function(x){ 
    out <- 0*diag( n.vars * p )
    out[1:n.vars,1:n.vars] <- A[,x] %*% t(A[,x])
    return(out)
  } )
  big.sigma.extended <- 0*diag( n.vars * p ) ; big.sigma.extended[1:n.vars,1:n.vars] <- big.sigma
    # The lag-extended var-covar matrices
  big.sigma.denominator <- 0
    # Useful for the variance ratio
  
  for( i in 1:n.pds ){
    if( i==1 ){
      l.var.decomp[[i]] <- lapply( 1:n.vars, function(x) ((B %^% i) %*% A.sq.extended[[x]] %*% t(B %^% i))[1:n.vars,1:n.vars] )
    }else{
      l.var.decomp[[i]] <- lapply( 1:n.vars, function(x) l.var.decomp[[i-1]][[x]] + 
                                     ((B %^% i) %*% A.sq.extended[[x]] %*% t(B %^% i))[1:n.vars,1:n.vars] )
    }
    big.sigma.denominator <- big.sigma.denominator + ((B %^% i) %*% big.sigma.extended %*% t(B %^% i))[1:n.vars,1:n.vars]
    l.var.decomp.ratio[[i]] <- sapply( 1:n.vars, function(x) diag(l.var.decomp[[i]][[x]]) / 
                                       diag( big.sigma.denominator) )
    shk.names <- c( paste0( 'Sentiment #', 1:n.fcast), paste0( 'Fundamental #', 1:(n.vars-n.fcast) ) )
    colnames(l.var.decomp.ratio[[i]]) <- shk.names 
    #c( 'Non-fundamental', paste0( 'Fundamental #', 1:(n.vars-1) ))
  }
  
  return( list( var.decomp=l.var.decomp, decomp.ratio=l.var.decomp.ratio ) )
  
}

anc.analytic <- function( big.sigma, phi.h ){
# Solves for the analytical version of the structural identification
  
  if(is.null(phi.h %>% nrow())) phi.h <- phi.h %>% t
  nn <- dim( big.sigma )[2]
    # Number of variables total
  n.fcast <- phi.h %>% nrow()
    # The number of forecast variables
  phi.h.fcast <- phi.h[1:n.fcast, 1:n.fcast]
  phi.h.c <- phi.h[,-(1:n.fcast)]
  if(is.null(phi.h.c %>% nrow())) phi.h.c <- phi.h.c %>% t
    # Extract the phi.h components
  gamma <- solve( diag(n.fcast) - phi.h.fcast, phi.h.c )
    # A useful object
  
  sigma.11 <- big.sigma[ 1:n.fcast, 1:n.fcast  ]
  sigma.12 <- big.sigma[ 1:n.fcast, (1+n.fcast):nn  ]
  sigma.22 <- big.sigma[ (1+n.fcast):nn, (1+n.fcast):nn  ]
    # Extract Sigma submatrices
  
  V.S <- cbind( diag(n.fcast) - phi.h.fcast, -phi.h.c ) %*%
    big.sigma %*% t(cbind( diag(n.fcast) - phi.h.fcast, -phi.h.c ))
  m.Lambda <- chol(V.S) %>% t
    # Decomposing the sentiment innovation variance
  
  anc <- solve( m.Lambda, ( diag(n.fcast) - phi.h.fcast) %*% ( sigma.12 - gamma %*% sigma.22 ) ) %>% t
  anf <- solve( diag(n.fcast) - phi.h.fcast, m.Lambda ) + gamma %*% anc
  afc <- chol( sigma.22 - anc %*% t(anc) ) %>% t
  # afc <- afc.svd$u %*% diag(sqrt(afc.svd$d))
  aff <- gamma %*% afc
  m.A <- cbind( rbind( anf, anc ),
                rbind( aff, afc ) )
  rownames(m.A) <- rownames(big.sigma)
  colnames(m.A) <- c( paste0( 'Sentiment ', 1:n.fcast),
                      paste0( 'Fundamental ', 1:(nn-n.fcast) ) )
    # Solve for A
  sigma.err <- max( abs( big.sigma - (m.A %*% t(m.A)) ) )
  status <- if(sigma.err<1e-08) 'success' else 'failure'
    # Output processing
  return( list(A=m.A, sigma.err=sigma.err ))
  
}

A.reorder <- function( l.A, Sigma, n.fcast, fundamental.idx ){
# Reorders the rows of A
  
  if(fundamental.idx==0){
    return(l.A)
  }
  
  m.A <- l.A$A
  reorder.from.idx <- n.fcast + 1
  reorder.to.idx <- fundamental.idx
  
  m.trans <- diag(nrow(m.A)) ; 
  m.trans[reorder.from.idx,reorder.from.idx] <- m.trans[reorder.to.idx,reorder.to.idx] <- 0 ; 
  m.trans[reorder.from.idx,reorder.to.idx] <- m.trans[reorder.to.idx,reorder.from.idx] <- 1
  m.trans.sub <- m.trans[-(1:n.fcast),-(1:n.fcast)]
  
  m.Sigma <- Sigma
  m.Sigma.trans <- m.trans %*% m.Sigma %*% t(m.trans)
    # The target for the outcome
  m.A.trans <- m.trans %*% m.A
  m.A.trans.check <- m.A.trans %*% t(m.A.trans) - m.Sigma.trans
    # The initial transposed A matrix
  
  m.D <- m.A.trans[-(1:n.fcast),-(1:n.fcast)]
  m.D.sq <- m.D %*% t(m.D)
  m.D.tilde <- chol( m.D.sq ) %>% t()
  m.D.tilde.check <- m.D.sq - ( m.D.tilde %*% t(m.D.tilde) )
    # Partial out the new D
  m.B <- m.A.trans[1,-1]
  m.B.tilde <- (solve( m.D.tilde ) %*% m.D %*% m.B) %>% t
  m.B.tilde.check <- m.B.tilde ^2 %>% sum() - m.B ^2 %>% sum()
  
  m.A.alt <- m.A.trans ; m.A.alt[-(1:n.fcast),-(1:n.fcast)] <- m.D.tilde ; m.A.alt[1,-1] <- m.B.tilde
  m.Sigma.trans.check <- m.A.alt %*% t(m.A.alt) - m.Sigma.trans
  
  m.A.final <- m.trans %*% m.A.alt %>%
    set_colnames(colnames(m.A)) %>%
    set_rownames(rownames(m.A))
  m.A.final.check <- (m.A.final %*% t(m.A.final) - m.Sigma) %>% abs %>% max
  out <- list( A=m.A.final, sigma.err=m.A.final.check ) 
  
  return(out)
}

# anc.nlslv.robust <- function( big.sigma, phi.h, outer.iter=20, 
#                               anc=NA, tol=1e-08, do.nD=FALSE  ){
#   # Solves for the structural form decomposition
#   
#   if(do.nD){
#     
#     sol.0 <- anc.analytic()
#     if(sol.0$status=='success') return(sol.0)
#     
#   }
#   
#   first.sol <- anc.nlslv( big.sigma, phi.h, anc, tol, do.nD=do.nD )
#   # first.sol <- anc.nlslv( l.var.coefs$Sigma, est.phi.h, anc, tol )
#   if( first.sol$status == 'success' ) return(first.sol)
#   
#   this.status <- 'failure'
#   iter <- 0 ; this.sd <- .01 ; set.seed(42)
#   n.anc <- first.sol$anc %>% length()
#   this.sol <- first.sol ; this.sol$iter.min <- 0
#   
#   while( this.status =='failure' & iter < outer.iter){
#     this.anc <- rnorm(n.anc, 0, this.sd)
#     if(do.nD){
#       n.r <- first.sol$anc %>% nrow() ; n.c <- first.sol$anc %>% ncol()
#       this.anc <- matrix( this.anc, n.r, n.c )
#     }
#     # this.sol <- anc.nlslv( l.var.coefs$Sigma, est.phi.h, this.anc, tol )
#     # browser()
#     this.candidate <- anc.nlslv( l.var.coefs$Sigma, phi.h, this.anc, tol, do.nD=do.nD )
#     this.candidate$iter.min <- iter
#     if(this.candidate$err < this.sol$err) this.sol <- this.candidate
#     this.sd <- this.sd + .09 / outer.iter
#     this.status <- this.sol$status
#     iter <- iter + 1
#   }
#   this.sol$robust.iter <- iter
#   
#   return( this.sol )
# }




