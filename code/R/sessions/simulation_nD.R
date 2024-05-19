##########################################################################################
#
# simulation_nD.R
#
# Code to check the identification of multiple shocks using simulated data from a full NK model
# Philip Barrett, Washington DC
# First version: 15nov2023
#
##########################################################################################

#### 0. Set up ####
rm(list=ls())
source('code/R/functions/make_var_fns.R')
source('code/R/functions/make_irf_fns.R')
source('code/R/functions/make_non_RE_var_fns.R')
library(magrittr)
set.seed(42)

#### 1. Controls ####
freq <- 'm'
# The frequency of the eventual simulated data
max.est.lag <- if(freq=='m') 10 else 6
ann.pds <- if(freq=='m') 12 else 4
irf.data <- 'model_solutions//michigan_cpi_1982_m_3.rdata'
  # The empirical IRFs
fcast.in <- c('fcast.inf', 'fcast.y')
x.in <- c('inflation', 'output')
y.in <- 'fed.funds'
  # How the data come in
fcast <- fcast.in %>% rev
x <- x.in %>% rev
y <- y.in
  # The order we want for the structural identification
fcast.cumul <- c(FALSE, TRUE)
  # Cumulative forecast, in fcast.order order
inf.sentiment.names <- list( sim='inf.sentiment', 
                             A=(paste0( 'Sentiment ', which(x=='inflation') )),
                             irf=(paste0( 'Sentiment #', which(x=='inflation') )))
  # the names of the inflation sentiment
n.fcast <- fcast %>% length()
n.vars <- 2*n.fcast + length(y)
x.order <- c('output')
var.labs <- c( inflation='Inflation', fcast.inf='Inflation, forecast', 
               inflation.sentiment='Inflation, Sentiment', inflation.fire='Inflation, RE forecast', 
               output='Log output',fcast.y='Log output, forecast', 
               output.sentiment='Log output, Sentiment', output.fire='Log output, RE forecast', 
               fed.funds='Interest Rate')
save.file <- 'replication/'

#### 2. Read in the data ####
m.Y <- read_csv('./data/simulated/multiple_shocks/simdata.csv', 
                col_names =c( fcast.in, x.in, y) ) %>% 
  select( c(fcast, x, y ) ) %>%
  as.matrix()
n.sim.pds <- m.Y %>% nrow()
  # The length of the simulation
m.shocks.in <- read_csv('./data/simulated/multiple_shocks/simshocks.csv', 
                        col_names =c('nu', 'z', 'a', 'a.news', 'a.sentiment', 'inf.sentiment', 'y.sentiment') ) %>% 
  as.matrix()
m.shocks <- (m.shocks.in / ( ( m.shocks.in %>% nrow %>% rep(1, .) ) %*%
                              (m.shocks.in %>% var %>% diag() %>% sqrt %>% t) )) %>%
  tail( n.sim.pds )
# m.shocks <- m.shocks.in %>% tail( n.sim.pds )

df.irf.dynare <- read_csv( './data/simulated/multiple_shocks/simirfs.csv', 
                           col_names =c( fcast.in, x.in, y ) ) %>% 
  cbind( read_csv( './data/simulated/multiple_shocks/simirfs_more.csv', 
            col_names =c( paste0(x.in, '.fire'), paste0( x.in[1], '.sentiment'), 'drop.me' ) ) ) %>% 
  mutate(output.sentiment=0) %>%
  select( c(fcast, x, c( paste0(x, '.fire'), paste0( x, '.sentiment') ), y ) ) 
df.irf.dynare <- df.irf.dynare %>% mutate_all( function(x) x/ df.irf.dynare$inflation.sentiment[1] )

#### 3. Point estimates of the IRFs ####

## 3.1 The true(ish) structural impact from regressing on the actual shocks with multiple lag lengths ##
l.A.est <- lapply( 0:max.est.lag, function(j) 
  sapply( 1:ncol(m.Y), function(i){
    Y <- if(j==0) m.Y[,i] else ( VAR( m.Y, p=j ) %>% resid %>% .[,i] )
    # Partial out the VAR lags
    X <- if( j==0 ) m.shocks else m.shocks[-(1:j),]
    lm( Y ~ X ) %>% coef %>% .[paste0('X', colnames(m.shocks))] 
  } ) %>% t() %>% set_colnames( colnames(.) %>% gsub('X', '', .) ) %>%
    set_rownames(colnames(m.Y)) )


## 3.2 Estimate the reduced form from the simulated data ##
l.var.sim <- make.var( NA, NA, NA, fcast=fcast, 
                       inf=x, y=y, m.Y=m.Y, lag.max = max.est.lag )
i.lag.idx <- l.var.sim$lags + 1
  # Adding one because the first entry is zero lags

## 3.3 Select the true structural decomp from the appropriate VAR by lag length ##
m.A.est <- l.A.est[[i.lag.idx]]
  # Select the correct structural impact matrix

## 3.4 Estimate the structural decomp using the true solution as an IC ##
sim.phi.h <- sapply( 1:n.fcast,
                     function(i) make.phi.h( l.var.sim, horiz = ann.pds, 
                                             inf.idx = n.fcast + i, cumul=fcast.cumul[i] )$phi.h ) %>%
  set_colnames( fcast ) %>% t
A.sim <- anc.analytic( l.var.sim$Sigma, sim.phi.h )
# sim.phi.DO <- make.phi.h.DO( l.var.sim, horiz = ann.pds )
# A.sim.DO <- anc.analytic( l.var.sim$Sigma, sim.phi.DO$phi.h, anc=anc.0 )
# sim.phi.AE <- make.phi.h.AE.fix( l.var.sim, horiz = ann.pds, theta = .91 )
# A.sim.AE <- anc.nlslv( l.var.sim$Sigma, sim.phi.AE$phi.h, anc=anc.0 )

## 3.5 Check against the estimated parts of the VAR (etc.) ##
m.Sigma.est <- m.A.est %*% (m.shocks %>% var) %*% t(m.A.est)
# The reduced form variance-covariance matrix
Sigma.err <- ( l.var.sim$Sigma - m.Sigma.est ) %>% abs %>% max
  # The error on the reduced form 
sent.err <- max(abs(m.A.est[,inf.sentiment.names$sim] - A.sim$A[,inf.sentiment.names$A]))
  # The sentimental error

## 3.6 Plot the corresponding IRFs ##
n.sim.shk <- m.A.est %>% ncol()
n.sentiments.to.sum <- m.A.est %>% colnames %>% grepl('sentiment',.) %>% sum() - n.fcast + 1
n.shks.to.sum <- n.sim.shk - (n.sentiments.to.sum - 1) - n.vars + 1

m.A.est.square <- matrix( NA, n.vars, n.vars) %>%
  set_colnames(rep(NA,n.vars)) %>%
  set_rownames( c(fcast, x, y))
inf.sentiment.idx <- which(m.A.est %>% colnames %>% grepl(inf.sentiment.names$sim,.))
non.inf.sentiments.idx <- which( (m.A.est %>% colnames %>% grepl('sentiment',.)) ) %>%
  setdiff( ., inf.sentiment.idx )
if(n.sentiments.to.sum>0){
  cols.to.sum <- non.inf.sentiments.idx[1:n.shks.to.sum]
  other.sentiments.idx <- c( setdiff(cols.to.sum, non.inf.sentiments.idx), inf.sentiment.idx)
    # Order inflation at the end
  m.A.est.square[,1] <- rowSums(m.A.est[,cols.to.sum])
  m.A.est.square[,2:n.fcast] <- m.A.est[,other.sentiments.idx]
  colnames(m.A.est.square)[1:n.fcast] <- c( 'Composite sentiment', colnames(m.A.est)[other.sentiments.idx] )
}else{
  m.A.est.square[,1:n.fcast] <- m.A.est[,c(non.inf.sentiments.idx, inf.sentiment.idx)]
  colnames(m.A.est.square)[1:n.fcast] <- colnames(m.A.est)[c(non.inf.sentiments.idx, inf.sentiment.idx)]
}

not.sentiments <- which( !(m.A.est %>% colnames %>% grepl('sentiment',.) ) )
if( n.shks.to.sum > 0 ){
  cols.to.sum <- not.sentiments[1:n.shks.to.sum]
  other.cols <- setdiff( not.sentiments, cols.to.sum)
  m.A.est.square[,n.fcast+1] <- rowSums(m.A.est[,cols.to.sum])
  m.A.est.square[,(n.fcast+2):n.vars] <- m.A.est[,other.cols]
  colnames(m.A.est.square)[(n.fcast+1):n.vars] <- c( 'Composite fundamental', colnames(m.A.est)[other.cols] )
}else{
  m.A.est.square[,(n.fcast+1):n.vars] <- m.A.est
}
    # The square version of A. Need to drop the final shock as IRF code can only
    # handle n(shocks) = n(var).  Could also combine?
df.irf.struct.true <- make.irf.struct( l.var=l.var.sim, A=m.A.est.square, fcast.horiz=ann.pds, 
                                       inf.name=x, n.pds=3*ann.pds, fcast=fcast, fcast.cumul=fcast.cumul ) %>%
  group_by(shock) %>% mutate( value= value / value[period==0 & outcome=='inflation.sentiment'] )

df.irf.struct.sim <- make.irf.struct( l.var=l.var.sim, A=A.sim$A, fcast.horiz=ann.pds, 
                                      inf.name=x, n.pds=3*ann.pds, fcast=fcast, fcast.cumul=fcast.cumul ) %>%
  group_by(shock) %>% mutate( value= value / value[period==0 & outcome=='inflation.sentiment'] )

df.nf.struct <- rbind( df.irf.struct.true %>% filter(shock==inf.sentiment.names$irf) %>% mutate(Type='Est. Model') %>%
                         mutate(),
                       df.irf.struct.sim %>% filter(shock==inf.sentiment.names$irf) %>% 
                         mutate(Type=' Full sample estimation') ) %>%
  mutate( var.labels=var.labs[outcome] ) #,
  # The IRFs
gg.validation <- ggplot( df.nf.struct, # %>% filter( !(outcome %in% c('inf.sentiment', 'inf.fire')) ), 
                         aes( x=period, y=value, group=Type, lty=Type ) ) +
  geom_hline(yintercept=0) +
  geom_line(lwd=1.1) +
  facet_wrap(~var.labels, scales = 'free_y' ) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  xlim(c(0,2*ann.pds)) +
  xlab('Periods') + ylab('Percentage points')
gg.validation %>% print

ggsave( paste0( 'graphs/', save.file, 'validation_', freq, '_nD.pdf'), gg.validation, height=8, width=8 )


## 3.7 Add in the true IRFs from the save files ##
df.nf.struct.full <- full_join( df.nf.struct,
                                df.irf.dynare %>% mutate(Type='  Model', period=1:nrow(.)-1 ) %>%
                                  gather(outcome, value, -period, -Type) ) %>%
  mutate( var.labels=var.labs[outcome] )
# outcome.f=factor( outcome, levels=c('fcast', 'inf.fire', 'inf.sentiment', 'inflation', 'fed.funds', 'output') ) )
gg.validation.full <- ggplot( df.nf.struct.full, 
                              aes( x=period, y=value, group=Type, color=Type, shape=Type ) ) +
  geom_hline(yintercept=0) +
  geom_line(lwd=1.1) +
  geom_point(size=3) +
  facet_wrap(~var.labels, scales='free_y') + 
  theme_minimal() +
  theme(legend.position = 'bottom') +
  xlim(c(0,2*ann.pds)) +
  xlab('Periods') + ylab('Percentage points')
gg.validation.full %>% print

gg.validation.show <- ggplot( df.nf.struct.full %>% filter(!(Type=='Est. Model')), 
                              aes( x=period, y=value, group=Type, color=Type, shape=Type ) ) +
  geom_hline(yintercept=0) +
  geom_line(lwd=1.1) +
  geom_point(size=3) +
  facet_wrap(~var.labels, scales='free_y') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_shape_manual(values=c(18, 17, 16, 15)) +
  xlim(c(0,2*ann.pds)) +
  xlab('Periods') + ylab('Percentage points')
gg.validation.show %>% print
ggsave( paste0( 'graphs/',save.file,'validation_cholesky_ae_', freq, '.pdf'), gg.validation.show, height=8, width=10 )



#### 4. Now do on repeated shorter samples ####
init.yr <- 1982 ; final.yr <- 2022
n.pds.sample <- ( final.yr - init.yr ) * ann.pds
  # The number of periods to sample
n.samples.poss <- nrow(m.Y) / n.pds.sample %>% floor
  # The number of samples available in total
n.sample <- 1000
  # The number of samples to take
sample.starts <- (1:n.sample * ( nrow(m.Y) - 2 * n.pds.sample ) / n.sample) %>% floor
  # The initial start points for the samples

l.irf.nf <- list() ; i.counter <- 1 ; anc.0 <- m.A.est[-1,1] 
for( i in 1:n.sample ){
  
  message( 'Sample ', i )
  
  ## SET UP THE SAMPLE
  this.sample <- m.Y[ sample.starts[i] + 1:n.pds.sample, ]
    # The actual sample to estimate from
  this.var.sim <- make.var( NA, NA, NA, fcast=fcast, 
                            inf=x, y=y, m.Y=this.sample, lag.max = max.est.lag )
  this.sim.phi.h <- sapply( 1:n.fcast,
                            function(i) make.phi.h( this.var.sim, horiz = ann.pds, 
                                                    inf.idx = n.fcast + i, cumul=fcast.cumul[i] )$phi.h ) %>%
    set_colnames( fcast ) %>% t
    # The RF and targets
  this.A.sim <- anc.analytic( this.var.sim$Sigma, this.sim.phi.h )
  l.irf.nf[[i]] <- suppressMessages( 
    make.irf.struct( l.var=this.var.sim, A=this.A.sim$A, fcast.horiz=ann.pds, 
                     inf.name=x, n.pds=3*ann.pds, fcast=fcast, fcast.cumul=fcast.cumul ) ) %>%
    group_by(shock) %>% mutate( value= value / value[period==0 & outcome=='inflation.sentiment'] )
  
}

df.irf.nf.samples <- l.irf.nf %>% reduce( full_join ) %>%
  filter(shock==inf.sentiment.names$irf) %>%
  group_by( period, outcome ) %>% 
  summarise( q.005=quantile(value, .005, na.rm=T), q.025=quantile(value, .025, na.rm=T), 
             q.05=quantile(value, .05, na.rm=T), q.50=quantile(value, .50, na.rm=T),
             q.mean=mean(value, na.rm=T),
             q.95=quantile(value, .95, na.rm=T), q.975=quantile(value, .975, na.rm=T),
             q.995=quantile(value, .995, na.rm=T) )
# Reassemble the samples
df.nf.struct.all <- df.irf.nf.samples %>% 
  rename( value=q.50 ) %>% 
  select( -contains('q.')) %>% 
  mutate( Type='Short sample median' ) %>%
  full_join( df.nf.struct.full %>% filter( Type != 'Est. Model') ) %>%
  mutate(var.labels=var.labs[outcome])
save( df.irf.nf.samples, df.irf.nf.samples, df.nf.struct.all, file=paste0('data/sim_irf_', freq, '_nD.rdata' ) )

gg.validation.range <- ggplot( df.nf.struct.all %>% 
                                 mutate(value=ifelse(outcome %in% c( 'fed.funds', 'inflation'), 
                                                     value * ann.pds, value ) ) ) +
  geom_ribbon( data = df.irf.nf.samples %>%
                 mutate(q.05=ifelse(outcome %in% c( 'fed.funds', 'inflation'), q.05 * ann.pds, q.05 ),
                        q.95=ifelse(outcome %in% c( 'fed.funds', 'inflation'), q.95 * ann.pds, q.95 ),
                        var.labels=var.labs[outcome] ),
               aes(x=period, ymin=q.05, ymax=q.95 ), alpha=.1 ) +
  geom_hline(yintercept=0) +
  geom_line(lwd=.9, aes( x=period, y=value, group=Type, color=Type )) +
  geom_point(aes( x=period, y=value, group=Type, shape=Type, color=Type ), size=3) +
  facet_wrap(~var.labels, scales='free_y' ) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  xlim(c(0,2*ann.pds)) +
  xlab('Months') + ylab('') + labs( col=NULL, shape=NULL )
gg.validation.range %>% print
ggsave( paste0( 'graphs/', save.file, 'validation_range_', freq, '_nD.pdf'), gg.validation.range, height=8, width=10 )

#### 5. Compare to the estimated IRFs ####
load( irf.data )
  # The estimates

df.nf.chart.est <- df.irf.struct %>%
  filter(shock=='Sentiment #1' ) %>%
  select(-outcome.lab) %>%
  spread(outcome,value) %>%
  rename(c( fcast.inf='michigan.fcast', inf='inf.cpi', output='log.ip', 
            fed.funds='fed.funds.rate', inflation.fire='inf.cpi.fire', 
            inflation.sentiment=inf.cpi.sentiment )) %>%
  mutate( inflation=inf * ann.pds ) %>%
  select( period, fcast.inf, inflation, output, fed.funds, inflation.fire, inflation.sentiment ) %>%
  gather( outcome, value, -period ) %>%
  mutate( var.labels=var.labs[outcome] )

shock.scalar <- df.nf.struct.all %>% filter(outcome=='inflation.sentiment', period==0, Type=='  Model') %>% pull(value) /
  df.nf.chart.est %>% filter(outcome=='inflation.sentiment', period==0 ) %>% pull(value)

gg.validation.range.x <- ggplot( df.nf.struct.all %>% 
                                   mutate(value=ifelse(outcome %in% c( 'fed.funds', 'inflation'), value * ann.pds, value ) / shock.scalar ) ) +
  # geom_ribbon( data = df.irf.nf.samples %>%
  #                mutate(q.05=ifelse(outcome %in% c( 'fed.funds', 'inflation'), q.05 * ann.pds, q.05 ) / shock.scalar,
  #                       q.95=ifelse(outcome %in% c( 'fed.funds', 'inflation'), q.95 * ann.pds, q.95 ) / shock.scalar, 
  #                       outcome.f=as.factor(outcome) ) , 
  #              aes(x=period, ymin=q.05, ymax=q.95 ), alpha=.1 ) +
  geom_hline(yintercept=0) +
  geom_line(lwd=.9, aes( x=period, y=value, group=Type, color=Type )) +
  geom_line( data=df.nf.chart.est, aes(x=period, y=value), lty=2, lwd=1.1 ) +
  geom_point(aes( x=period, y=value, group=Type, shape=Type, color=Type ), size=3) +
  facet_wrap(~var.labels, scales='free_y') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_x_continuous( breaks=seq(0,2*ann.pds,ann.pds/2), limits = c(0,2*ann.pds) ) +
  xlab('Months') + ylab('') + labs( col=NULL, shape=NULL )
gg.validation.range.x %>% print
ggsave( paste0( 'graphs/', save.file, 'validation_range_', freq, '_x.pdf'), gg.validation.range.x, height=6, width=10 )


df.irf.compare <- df.nf.struct.all %>% filter(Type=='  Model', outcome %in% (df.nf.chart.est$outcome %>% unique()) ) %>%
  mutate(value=ifelse(outcome %in% c( 'fed.funds', 'inflation'), value * ann.pds, value ) / shock.scalar ) %>%
  full_join( df.nf.chart.est %>% mutate(Type='Estimated impulse responses') )

gg.comparison <- ggplot( df.irf.compare ) +
  geom_hline(yintercept=0) +
  geom_line(lwd=.9, aes( x=period, y=value, group=Type, color=Type )) +
  geom_point(aes( x=period, y=value, group=Type, shape=Type, color=Type ), size=3) +
  facet_wrap(~var.labels, scales='free_y') +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_x_continuous( breaks=seq(0,2*ann.pds,ann.pds/2), limits = c(0,2*ann.pds) ) +
  xlab('Months') + ylab('') + labs( col=NULL, shape=NULL )
gg.comparison %>% print
ggsave( paste0( 'graphs/', save.file, 'comparison_', freq, '.pdf'), gg.comparison, height=6, width=10 )



