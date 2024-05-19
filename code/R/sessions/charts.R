##########################################################################################
#
# charts.R
#
# Code to make charts for the "Inflation Sentiments" project
# Philip Barrett, Washington DC
# First version: 26jan2022, major rework for R&R 04nov2023
#
##########################################################################################

library(readxl)

#### 1. Come controls ####
var.labs <- c( cleveland.fcast='Cleveland Inflation Forecast', 
               # michigan.fcast='Michigan Inflation Forecast',
               fed.fcast='Fed Green Book Forecast',
               fed.funds.rate='Federal Funds Rate', inf.commod='Commodity price inflation', 
               inf.pce='PCE growth', ip.gth='Industrial production growth',
               m2.gth='M2 Growth', unemp=' Unemployment', fed.funds.diff='Change in Fed Funds rate', 
               unemp.diff='Change in Unemployment rate', 
               ind.prod='Industrial production', 
               spf.fcast='  Inflation, one year ahead \ncummulative,  SPF forecast', 
               spf.fcast.unemp=' Unemployment, one year ahead, \n  SPF forecast', 
               unemp=' Unemployment', 
               unemp.fire=' Unemployment, one year ahead', 
               unemp.sentiment=' Unemployment, sentiment', 
               michigan.fcast='  Inflation, one year ahead \ncummulative, forecast', 
               michigan.fcast.trend='  Inflation, one year ahead \ncummulative, forecast', 
               spf.fcast.log.gdp=' Log GDP * 100,  SPF forecast', 
               log.ip=' Log Industrial Production * 100',
               log.ip.fcast=' Log Industrial Production * 100, \nforecast', 
               log.ip.fire=' Log Industrial Production * 100, \none year ahead',
               log.ip.sentiment=' Log Industrial Production * 100, \nsentiment',
               log.real.pi=' Log real personal income * 100',
               log.real.pi.fcast=' Log real personal income * 100, \nforecast', 
               log.real.pi.fire=' Log real personal income * 100, \none year ahead',
               log.real.pi.sentiment=' Log real personal income * 100, \nsentiment',
               log.p.oil='Log Oil Price * 100',
               inf.oil='Oil Price Inflation',
               log.gdp=' Log GDP * 100', 
               log.gdp.fire=' Log GDP * 100, one year ahead', 
               log.gdp.sentiment=' Log GDP * 100, sentiment', 
               log.pce=' Log PCE * 100', 
               log.pce.fire=' Log PCE * 100, one year ahead', 
               log.pce.sentiment=' Log PCE * 100, sentiment', 
               log.pce.fcast=' Log PCE * 100, forecast', 
               spf.fcast.irate=' Three-Month treasury bill rate, \n SPF forecast',
               tbill=' Three-Month treasury bill rate',
               tbill.fire=' Three-Month treasury bill rate, \none year ahead',
               tbill.sentiment=' Three-Month treasury bill rate, \nsentiment',
               log.cons='Log Consumption * 100', 
               inf.cpi='  Inflation, annual rate', 
               inf.cpi.fire='  Inflation, one year ahead \ncumulative', 
               inf.cpi.sentiment='  Inflation, one year ahead \ncumulative, sentiment',
               inf.gdp='  Inflation, annual rate', log.inv='Log investment * 100', 
               inf.gdp.fire='  Inflation, one year ahead \ncumulative', 
               inf.gdp.sentiment='  Inflation, one year ahead \ncumulative, sentiment',
               inf.cpi.first='  Inflation, annual rate, initial release', 
               inf.cpi.first.fire='  Inflation, one year ahead \ncumulative, initial release', 
               inf.cpi.first.sentiment='  Inflation, one year ahead \ncumulative, sentiment, initial release',
               i.3.mo='3-month treasury rate', i.1.yr='1-year treasury rate',
               i.2.yr='2-year treasury rate', i.5.yr='5-year treasury rate',
               i.10.yr='10-year treasury rate', i.20.yr='20-year treasury rate', i.30.yr='30-year treasury rate',
               log.m1='100 * log M1', log.m2='100 * log M2', log.currency='Log currency * 100', 
               log.bank.credit='100 * log bank credit', leverage.idx='Leverage index', 
               log.wk.hrs='100 * Log work hours', log.vehicles='100 * Log vehicle sales',
               log.emp='Log employment * 100', log.housing.starts='100 * Log housing Starts',
               log.p.oil.r='Log real oil price * 100', g.surp.idx='Primary surplus index', 
               log.will.idx='Log Wilshire 5000 index * 100', 
               log.fx.cad='100 * Log USD-CAD exch. rate',
               log.fx.jpn='100 * Log USD-JPY exch. rate', log.fx.gbr='100 * Log USD-GBP exch. rate' )
l.outcomes <- list( base=c(fcast, x, 'fed.funds.rate', 'log.ip' ),
                    int.rates=c('fed.funds.rate', 'i.1.mo', 'i.3.mo', 'i.1.yr', 'i.2.yr', 'i.5.yr', 'i.10.yr', 'i.20.yr', 'i.30.yr'),
                    money.credit=c('log.m1', 'log.m2', 'log.currency', 'log.bank.credit','leverage.idx', paste0(x, '.a') ),
                    real=c('log.vehicles', 'unemp', 'log.wk.hrs', 'log.emp', 'log.ip', 'log.housing.starts', 'log.r.oil.r'),
                    financial=c('g.surp.idx', 'log.will.idx', 'log.fx.cad', 'log.fx.gbr', 'log.fx.jpn' ) )
annualize.vars <- c( 'inf.cpi', 'inf.gdp' )

pd <- if(freq=='q') 'quarter' else 'mon'


#### 1. Plot the time series ####
gg.ts.data <- 
    ggplot( df %>% filter(variable %in% names(var.labs)) %>% 
              gather(seas, value, -date, -variable, -pd ),
            aes(x=date, y=value, group=seas, color=seas)) +
      geom_line(lwd=.9) +
      facet_wrap(~variable, scale='free_y', label=labeller( variable=var.labs ) ) +
      theme_minimal() +
      scale_color_manual( labels=c( 'Raw data', 'Non-seasonal component'), values=c('blue', 'red') ) +
      labs( color='Type', y='Percent') +
      theme( legend.position = 'bottom' )
ggsave( paste0('graphs/', figure.location, '/time_series_', freq, '_all.pdf'), gg.ts.data, height=10, width=10 )

gg.ts.data.ds <- 
  ggplot( df %>% filter(variable %in% names(var.labs)),
          aes(x=date, y=value.deseas)) +
  geom_line(lwd=.9) +
  facet_wrap(~variable, scale='free_y', label=labeller( variable=var.labs ) ) +
  theme_minimal() +
  labs( color='Type', y='Percent') +
  theme( legend.position = 'bottom' )
ggsave( paste0('graphs/', figure.location, '/time_series_deseas_', init.yr, '_', freq,  '_all.pdf'), gg.ts.data.ds, height=10, width=10 )

if( freq=='m'){
  for( gp in names(l.outcomes) ){
    
    this.gg.ts.data <- 
      ggplot( df %>% filter(variable %in% l.outcomes[[gp]]) %>% 
                gather(seas, value, -date, -variable, -pd ),
              aes(x=date, y=value, group=seas, color=seas)) +
      geom_line(lwd=.9) +
      facet_wrap(~variable, scale='free_y', label=labeller( variable=var.labs ) ) +
      theme_minimal() +
      scale_color_discrete( labels=c( 'Raw data', 'Non-seasonal component')) + 
      scale_linetype_discrete( labels=c( 'Raw data', 'Non-seasonal component')) + 
      labs( color='Type', linetype='Type', y='Percent', x='Date') +
      geom_hline(aes(yintercept=0), lwd=.5) +
      theme( legend.position = 'bottom' )
    ggsave( paste0('graphs/', figure.location, '/time_series_', freq, '_', gp %>% gsub('\\.','_',.), '.pdf'), this.gg.ts.data, height=6, width=10 )
    
    gg.ts.data.ds <- 
      ggplot( df %>% filter(variable %in% l.outcomes[[gp]]),
              aes(x=date, y=value.deseas)) +
      geom_line(lwd=.9) +
      facet_wrap(~variable, scale='free_y', label=labeller( variable=var.labs ) ) +
      theme_minimal() +
      labs( color='Type', y='Percent') +
      geom_hline(aes(yintercept=0), lwd=.5) +
      theme( legend.position = 'bottom' )
    ggsave( paste0('graphs/', figure.location, '/time_series_deseas_', init.yr, '_', freq, '_', gp %>% gsub('\\.','_',.),  '.pdf'), gg.ts.data.ds, height=6, width=10 )
  }    
}

#### 2. Plot the IRFs ###

## 2.1 Reduced form ##
df.irf.rf <- make.irf.rf(l.var.coefs, n.pds = irf.pds )
gg.irfs.rf <- ggplot( df.irf.rf ) +
                      geom_path( aes(x=period, y=value), lwd=.9) +
                      geom_ribbon( data = est.bootstrap$reduced.form, aes(x=period, ymin=q.05, ymax=q.95 ), alpha=.25 ) +
                      facet_grid(rows = vars(shock), cols=vars(outcome), label=labeller( shock=var.labs, outcome=var.labs )) +
                      theme_minimal() +
                      labs( title=paste0('Reduced form impulse responses, ', l.var.coefs$lags, ' lags') ) +
                      xlab('Period') + ylab('Percent') +
                      xlim(c(0,irf.plot.pds))
ggsave( paste0( 'graphs/', figure.location, '/irf_rf_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags.pdf'), gg.irfs.rf, height = 10, width = 10)

## 2.2 Structural ##
df.irf.struct <- make.irf.struct(l.var.coefs, est.A$A, fcast.horiz=fcast.horiz, 
                                 inf.name=x, n.pds = irf.pds, fcast = fcast,
                                 fcast.cumul=fcast.cumul )
other.vars <- df.irf.struct %>% pull(outcome) %>% unique %>% .[!(.==fcast)]
df.irf.struct <- df.irf.struct %>% mutate( outcome.lab=var.labs[outcome] )
gg.irfs.struct <- ggplot( df.irf.struct ) +
                          geom_path(lwd=.9, aes(x=period, y=value)) +
                          geom_ribbon( data = est.bootstrap$structural %>% 
                                         mutate( outcome.lab=var.labs[outcome] ),
                                       aes(x=period, ymin=q.05, ymax=q.95 ), alpha=.25 ) +
                          facet_grid(rows = vars(shock), cols=vars(outcome.lab)) +
                          theme_minimal() +
                          xlim(c(0,irf.plot.pds)) +
                          labs( title=paste0('Structural impulse responses, ', l.var.coefs$lags, ' lags') ) +
                          xlab('Period') + ylab('Percent')
ggsave( paste0( 'graphs/', figure.location, '/irf_struct_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags.pdf'), gg.irfs.struct, height = 10, width = 10)

irf.ylim <- if(freq=='m') c(-.8, .4) else c(-2,2)
annualize <- if(freq=='m') function(x) x*12 else function(x) x*4

for( this.fcast in 1:n.fcast ){
  df.irf.nf <- df.irf.struct %>% filter(str_detect(shock, paste0('Sentiment #', this.fcast))) %>% 
    mutate( outcome.lab=var.labs[outcome],
            value=ifelse(outcome %in% annualize.vars, annualize(value), value) )
  gg.irfs.struct.nf <- ggplot( df.irf.nf ) +
    geom_hline(aes(yintercept=0), lwd=.5) +
    geom_path(lwd=.9, aes(x=period, y=value)) +
    geom_ribbon( data = est.bootstrap$structural %>% 
                   filter(str_detect(shock, paste0('Sentiment #', this.fcast))) %>%
                   mutate( outcome.lab=var.labs[outcome],
                           q.05=ifelse(outcome %in% annualize.vars, annualize(q.05), q.05),
                           q.95=ifelse(outcome %in% annualize.vars, annualize(q.95), q.95) ),
                 aes(x=period, ymin=q.05, ymax=q.95 ), alpha=.25 ) +
    # facet_grid(rows = vars(shock), cols=vars(outcome.f), label=labeller( outcome.f=var.labs )) +
    facet_wrap(~outcome.lab, scales='free_y') + #, label=labeller( outcome.f=var.labs )) +
    theme_minimal() +
    # labs( title=paste0('Structural impulse responses, ', l.var.coefs$lags, ' lags') ) +
    xlab('Horizon') + ylab('Percent points') +
    scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) )
  gg.irfs.struct.nf %>% print
  ggsave( paste0( 'graphs/', figure.location, '/irf_struct_nf_', this.fcast, 
                  '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags.pdf'), 
          gg.irfs.struct.nf, height = 8, width = 10)
}

n.fund <- n.vars - n.fcast
for( this.fund in 1:n.fund ){
  df.irf.fund <- df.irf.struct %>% filter(str_detect(shock, paste0('Fundamental #', this.fund))) %>% 
    mutate( outcome.lab=var.labs[outcome],
            value=ifelse(outcome %in% annualize.vars, annualize(value), value) )
  gg.irfs.struct.fund <- ggplot( df.irf.fund ) +
    geom_hline(aes(yintercept=0), lwd=.5) +
    geom_path(lwd=.9, aes(x=period, y=value)) +
    geom_ribbon( data = est.bootstrap$structural %>% 
                   filter(str_detect(shock, paste0('Fundamental #', this.fund))) %>%
                   mutate( outcome.lab=var.labs[outcome],
                           q.05=ifelse(outcome %in% annualize.vars, annualize(q.05), q.05),
                           q.95=ifelse(outcome %in% annualize.vars, annualize(q.95), q.95) ),
                 aes(x=period, ymin=q.05, ymax=q.95 ), alpha=.25 ) +
    # facet_grid(rows = vars(shock), cols=vars(outcome.f), label=labeller( outcome.f=var.labs )) +
    facet_wrap(~outcome.lab, scales='free_y') + #, label=labeller( outcome.f=var.labs )) +
    theme_minimal() +
    # labs( title=paste0('Structural impulse responses, ', l.var.coefs$lags, ' lags') ) +
    xlab('Horizon') + ylab('Percent points') +
    scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) )
  gg.irfs.struct.fund %>% print
  ggsave( paste0( 'graphs/', figure.location, '/irf_struct_fund_', this.fund, 
                  '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags.pdf'), 
          gg.irfs.struct.fund, height = 8, width = 10)
}


if(do.non.re){
  
  for( this.fcast in 1:n.fcast ){
    df.irf.struct.non.re <- lapply( names(l.est.A.non.re), function(nn)
      make.irf.struct(l.var.coefs, l.est.A.non.re[[nn]]$A, fcast.horiz=fcast.horiz, inf.name=x,
                      n.pds = irf.pds, fcast = fcast ) %>%
        mutate( expectations=nn ) ) %>%
      reduce(full_join) %>%
      filter(str_detect(shock, paste0('Sentiment #', this.fcast))) %>% 
      mutate( outcome.lab=var.labs[outcome],
              value=ifelse(outcome %in% annualize.vars, annualize(value), value) ) %>%
      # mutate(value=ifelse(outcome %in% annualize.vars, annualize(value), value),
      #        outcome.f=ifelse(outcome==x,paste0(x,'.a'),outcome) %>% as.factor,
      #        outcome.f = fct_relevel( outcome.f, c(fcast, 'inf.fire', 'inf.sentiment', paste0(x, '.a'), y ) ) ) %>%
      full_join( df.irf.nf %>% mutate(expectations='RE') ) %>%
      mutate( expectations.f=as.factor(expectations) )
    gg.irfs.non.re <- ggplot( df.irf.struct.non.re, aes(x=period, y=value, group=expectations.f, 
                                                        color=expectations.f, shape=expectations.f,
                                                        fill=expectations.f )) +
      geom_path(lwd=.9) +
      geom_hline(aes(yintercept=0), lwd=.5) +
      geom_point(size=3) +
      scale_shape_manual(values=1:nlevels(df.irf.struct.non.re$expectations.f))  +
      facet_wrap( ~outcome.lab, scales='free_y' ) + #, label=labeller( outcome.f=var.labs )) +
      # xlim(c(0,irf.plot.pds)) +
      theme_minimal() + # ylim(irf.ylim) +
      xlab('Horizon') + ylab('Percentage points') +
      theme(legend.position = 'bottom') +
      # scale_shape_manual(values = c(15, 17, 3, 4, 16) ) + 
      scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) )
    gg.irfs.non.re %>% print()
    ggsave( paste0( 'graphs/', figure.location, '/irf_struct_nf_', this.fcast, 
                    '_', init.yr , '_', freq, '_', l.var.coefs$lags, '_lags_non_RE.pdf'), 
            gg.irfs.non.re, height = 8, width = 10)
    
    gg.impact.non.re <- ggplot( df.irf.struct.non.re %>% filter(period==0),
                                aes(x=expectations.f, group=expectations.f, shape=expectations.f, 
                                    fill=expectations.f, color=expectations.f ) ) +
      geom_point(size=2, aes(y=value)) +
      scale_shape_manual(values=1:nlevels(df.irf.struct.non.re$expectations.f))  +
      # geom_errorbar(aes(ymin = q.05, ymax = q.95), width = 0.1) +
      geom_hline(aes(yintercept=0), lwd=.5) +
      facet_wrap( ~outcome.lab, scales='free_y') +
      theme_minimal() + # ylim(irf.ylim) +
      xlab('') + ylab('') +
      theme(legend.position = 'bottom') +
      labs(color='Expectations', shape='Expectations', fill='Expectations') +
      theme(axis.text.x = element_blank())
    gg.impact.non.re %>% print
    ggsave( paste0( 'graphs/', figure.location, '/impact_struct_nf_', this.fcast, 
                    '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags_non_RE.pdf'), 
            gg.impact.non.re, height = 8, width = 10)
    
    df.impact.non.re.show <- df.irf.struct.non.re %>% 
      full_join( df.irf.nf %>% mutate(expectations.f='FIRE') ) %>%
      filter( !(outcome %in% c('inf.commod', 'unemp') ) ) %>%
      filter(period==0, expectations.f %in% c(l.non.re.show, 'FIRE') ) %>%
      mutate( expectations.f=fct_relevel(expectations.f, c(l.non.re.show, 'FIRE') ) )
    non.re.var.labs <- var.labs
    non.re.var.labs['inf.sentiment'] <- 'Predictable Inflation Error'
    gg.impact.non.re.show <- ggplot( df.impact.non.re.show,
                                aes(x=expectations.f, group=expectations.f, shape=expectations.f, 
                                    fill=expectations.f, color=expectations.f ) ) +
      geom_point(size=2, aes(y=value)) +
      # scale_shape_manual(values=1:nlevels(df.irf.struct.non.re$expectations.f))  +
      # geom_errorbar(aes(ymin = q.05, ymax = q.95), width = 0.1) +
      geom_hline(aes(yintercept=0), lwd=.5) +
      facet_wrap( ~outcome.lab, scales='free_y') +
      theme_minimal() + # ylim(irf.ylim) +
      xlab('') + ylab('') +
      theme(legend.position = 'bottom') +
      scale_shape_manual(values=c( 4, 21, 24, 22, 23, 25, 1))+
      labs(color='', shape='', fill='') +
      theme(axis.text.x = element_blank())
    gg.impact.non.re.show %>% print
    ggsave( paste0( 'graphs/', figure.location, '/impact_struct_nf_', this.fcast, 
                    '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags_non_RE_show.pdf'), 
            gg.impact.non.re.show, height = 8, width = 10)
  
  if(non.re.ci){
      df.impact.non.re.ci <- 
        lapply( 1:n.non.re, function(i) l.est.bootstrap.non.re[[i]]$structural %>%
                  mutate(expectations=names(l.non.re.fns)[i] ) ) %>% 
          reduce(rbind) %>%
          full_join( est.bootstrap$structural %>% 
                       mutate(expectations='FIRE' ) ) %>% 
          filter(str_detect(shock, paste0('Sentiment #', this.fcast)), period==0,
                 !(outcome %in% c('inf.commod', 'unemp') ) ) %>%
          mutate( expectations.f=fct_relevel(expectations %>% as.factor,
                                          c(l.non.re.show, 'FIRE') ),
                  # outcome.f=ifelse(outcome==x,paste0(x,'.a'),outcome) %>% as.factor,
                  q.05=ifelse(outcome %in% annualize.vars, annualize(q.05), q.05), 
                  q.95=ifelse(outcome %in% annualize.vars, annualize(q.95), q.95) ) %>%
        mutate( outcome.lab=var.labs[outcome] )
      gg.impact.non.re.show.ci <- gg.impact.non.re.show +
        geom_errorbar(data=df.impact.non.re.ci %>% 
                        filter(expectations %in% c(l.non.re.show, 'FIRE'),
                               str_detect(shock, paste0('Sentiment #', this.fcast)) ),
                                aes( ymin=q.05, ymax=q.95 ), width=0 )
      gg.impact.non.re.show.ci %>% print()
      ggsave( paste0( 'graphs/', figure.location, '/impact_struct_nf_', this.fcast, 
                      '_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags_non_RE_show_ci.pdf'), 
              gg.impact.non.re.show.ci, height = 8, width = 10)
    }
  }
}


if( do.lp ){
  est.lp.fire <- est.lp %>%
    spread( outcome, value ) %>%
    mutate( inf.cpi.fire=c( rollsum(inf.cpi,fcast.horiz,
                                  align='left', na.pad = TRUE)[-1], NA),
            inf.cpi.sentiment=michigan.fcast-inf.cpi.fire ) %>%
  gather(outcome, value, -period )
  df.var.lp <- left_join( df.irf.nf %>% rename( var=value), 
                          est.lp.fire %>% rename( lp=value) ) %>%
    mutate(outcome.lab=var.labs[outcome],
           lp=ifelse(outcome%in% annualize.vars, annualize(lp), lp)) %>%
    gather( est.type, value, -shock, -outcome, -period, -outcome.lab, -outcome.lab) %>%
    mutate( est.type=ifelse(est.type=='var', 'VAR', 'Local projection')) %>%
    as_tibble()
  gg.irfs.var.lp <- ggplot( df.var.lp, aes(x=period, y=value, group=est.type,
                                           color=est.type, shape=est.type)) +
      geom_path(lwd=.9) +
      geom_point(size=3) +
      geom_hline(aes(yintercept=0), lwd=.5) +
      facet_wrap( ~outcome.lab, scales='free_y') +
      theme_minimal() + # ylim(irf.ylim) +
      # labs( title=paste0('Structural impulse responses to non-fundamental shock only, ', l.var.coefs$lags, ' lags') ) +
      xlab('Horizon') + # ylab('Percent') +
      theme(legend.position = 'bottom') +
      labs(color='Estimation method', shape='Estimation method') +
      scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) )
  gg.irfs.var.lp %>% print
  ggsave( paste0( 'graphs/', figure.location, '/irf_var_lp_', init.yr, '_', freq, '_', l.var.coefs$lags, '_lags.pdf'), gg.irfs.var.lp, height = 10, width = 10)  
}


#### 3. Plot the variance decomposition ####
df.var.decomp <- 
  suppressMessages(
  lapply( 1:length(est.var.decomp$decomp.ratio), 
        function(i) est.var.decomp$decomp.ratio[[i]] %>% 
          as.data.frame %>%
          rownames_to_column('outcome') %>%
          gather( shock, value, -outcome ) %>%
          mutate( shock = ifelse( str_detect(shock, 'Fundamental'), 'Fundamental', shock)) %>%
          group_by( outcome, shock ) %>%
          summarise(value=sum(value)) %>%
          ungroup %>%
          mutate( horizon=i ) ) %>%
  reduce(full_join) %>%
  as_tibble() %>%
  select( outcome, shock, horizon, value ) %>%
    mutate(outcome.f = fct_relevel( outcome %>% as.factor, c(fcast, paste0(x, '.a'), y ) )) %>%
  arrange( outcome, shock, horizon ) )
gg.var.decomp <- ggplot( df.var.decomp, aes( group=shock ) ) +
                  geom_line(lwd=.9, aes(x=horizon, y=value, color=shock ) ) +
                  geom_point(size=3, aes(x=horizon, y=value, color=shock, shape=shock ) ) +
                  geom_ribbon( data=est.bootstrap$var.decomp %>%
                                 mutate(outcome.f=outcome %>% as_factor ),
                               aes( x=horizon, ymin=q.05, ymax=q.95, fill=shock ), alpha=.25 ) +
                  facet_wrap( ~outcome.f, label=labeller( outcome.f=var.labs ) ) +
                  theme_minimal() +
                  theme(legend.position = 'bottom') +
                  # xlim(c(0,irf.plot.pds)) +
                  labs(color='Shocks', shape='Shocks') + # , title=paste0('Structural variance decomposition, ', l.var.coefs$lags, ' lags') ) +
                  xlab('Horizon') + guides(fill='none') +
                  scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) ) +
                  ylim(c(0,1)) + 
                  ylab('Fraction of variance')
gg.var.decomp %>% print()
ggsave( paste0( 'graphs/', figure.location, '/var_decomp_', l.var.coefs$lags, '_lags.pdf'),
        gg.var.decomp, height = 6, width = 10)



#### 4. The sentiment shock itself ####
struct.resid <- solve( est.A$A, l.var.coefs$var %>% resid() %>% t ) %>% t
df.struct.resid <- data.frame( date = df$date %>% unique() %>% sort, 
                               rbind( matrix( NA, nrow=l.var.coefs$lags, ncol=est.A$A%>%ncol ),
                                      struct.resid ) )
names(df.struct.resid) <- c( 'date', paste0( 'Sentiment #', 1:n.fcast), 
                             paste0( 'Fundamental #', 1:(n.vars-n.fcast ) ) )
df.struct.resid.long <- df.struct.resid %>% 
  gather(shock, value, -date) %>% 
  group_by(shock) %>%
  mutate( value.12m=rollmean(value, 12, fill=NA, align='r' ) )
for( this.fcast in 1:n.fcast ){
  gg.sentiment <- ggplot( df.struct.resid.long %>% 
                            # filter( date >= '2020-01-01') %>%
                            filter( shock==paste0( 'Sentiment #', this.fcast) ) %>%
                            gather( type, value, -date, -shock) %>% 
                            mutate( type= ifelse(type=='value', ' Sentiment', '12-month rolling average') %>%
                                      as.factor()), 
                          aes( x=date, y=`value`, group=type, color=type, size=type ) ) +
    geom_line(lwd=.9)+
    geom_hline(aes(yintercept=0)) +
    theme_minimal() +
    xlab('') +
    scale_color_manual(values=c('lightgrey',"black")) +
    scale_size_manual(values=c(1,1)) +
    ylab(paste0('Sentiment #', this.fcast) ) +
    theme( legend.position = 'bottom' ) +
    labs(color=NULL)
  gg.sentiment %>% print
  ggsave( paste0( 'graphs/', figure.location, '/sentiment_', this.fcast, '_shk_', l.var.coefs$lags, '_lags.pdf'),
          gg.sentiment, height = 5, width = 10)
}

  
df.quick.sentiment.in <- read_xls('data/Baseline Time Series/inflation_sentiments.xls', sheet = 'Monthly')
df.quick.sentiment <- df.quick.sentiment.in %>%
  select( date=DATE, michigan.sentiment=UMCSENT) %>%
  mutate( michigan.sentiment.12m = rollmean(michigan.sentiment, 12, na.pad = TRUE, align='r' ) ) %>%
  na.omit()

correl.logfile <- paste0( 'graphs/', figure.location, '/correl_log.txt')
write( '** Correlation with Michigan consumer sentiment **', file=correl.logfile, append=FALSE )
  
for( i.sentiment in 1:n.fcast ){

  df.est.sent <- df.struct.resid.long %>% 
    # filter( date >= '2020-01-01') %>%
    filter( shock==paste0( 'Sentiment #', i.sentiment ) )  %>%
    left_join(df.quick.sentiment)
  df.est.sent.cht <- df.est.sent %>% ungroup() %>%
    select(date, value.12m, michigan.sentiment.12m) %>%
    filter(!is.na(value.12m)) %>%
    gather( variable, value, -date ) %>%
    mutate( Series=ifelse(variable=='value.12m','Measured inflation sentiment', 
                          'Michigan consumer sentiment')) %>%
    group_by(variable) %>%
    mutate(value=(value-mean(value, na.rm=T))/sd(value, na.rm=T))
  
  write( paste0('\n\nEstimated shock #', i.sentiment, '\n Monthly'), file=correl.logfile, append=TRUE )
  sink(correl.logfile, append = TRUE)
  with( df.est.sent, cor.test(value, michigan.sentiment ) ) %>% print
  sink()
  write( paste0('\n Smoothed 12-monthly'), file=correl.logfile, append=TRUE )
  sink(correl.logfile, append = TRUE)
  with( df.est.sent, cor.test(value.12m, michigan.sentiment.12m ) ) %>% print
  sink()
  
  gg.sentiment.mich <- ggplot( df.est.sent.cht, 
                          aes( x=date, y=value, group=Series, color=Series, lty=Series ) ) +
    geom_line(lwd=.9)+
    geom_hline(aes(yintercept=0)) +
    theme_minimal() +
    xlab('') +
    ylab('Standard deviations from mean') +
    theme( legend.position = 'bottom' ) +
    labs(lty=NULL, color=NULL)
  ggsave( paste0( 'graphs/', figure.location, '/sentiment_vs_UMCSENT_', i.sentiment, 
                  '_shk_', l.var.coefs$lags, '_lags.pdf'),
          gg.sentiment.mich, height = 5, width = 10)
}

df.sent.reg <- df.struct.resid.long %>% 
  filter( grepl('Sentiment', shock)) %>% 
  mutate( shock=paste0( 'sent.', str_sub(shock, -1, -1) ) ) %>%
  gather( type, value, -date, -shock ) %>%
  mutate( type=ifelse(str_sub(type, -3, -1)=='12m', '12m', '1m') ) %>%
  full_join(df.quick.sentiment %>% gather(shock, value, -date) %>%
              mutate( type=ifelse(str_sub(shock, -3, -1)=='12m', '12m', '1m'), shock='michigan' ) ) %>%
  spread( shock, value)
lm( paste0( 'michigan ~', paste0( 'sent.', 1:n.fcast, collapse='+' ) ), 
    df.sent.reg %>% filter( type=='12m') ) %>% summary()
lm( paste0( 'michigan ~', paste0( 'sent.', 1:n.fcast, collapse='+' ) ), 
    df.sent.reg %>% filter( type=='1m') ) %>% summary()


#### 5. The rolling regressions ####
make.roll.chart <- function( df.roll, date.var, xlab, df.full ){
# Make the rolling window chart
    
  df.cht <- df.roll %>% 
    select( outcome, date.var, q.05, q.50, q.95 ) %>%
    group_by(outcome) %>%
    left_join(df.full) %>%
    mutate( q.05=ifelse(outcome %in% annualize.vars, annualize(q.05), q.05),
            q.95=ifelse(outcome %in% annualize.vars, annualize(q.95), q.95),
            q.50=ifelse(outcome %in% annualize.vars, annualize(q.50), q.50),
            q.50.full=ifelse(outcome %in% annualize.vars, annualize(q.50.full), q.50.full),
            q.50.avg=mean(q.50, na.rm=T),
            outcome.lab=var.labs[outcome],
            outcome.f=ifelse(outcome==x,paste0(x,'.a'),outcome) %>% as.factor,
            outcome.f = fct_relevel( outcome.f, c(fcast, 'inf.fire', 'inf.sentiment', paste0(x, '.a'), y ) ) )
    # The chart data
  gg.out <- ggplot(df.cht, aes_string(x=date.var) ) +
    geom_path( aes(y=q.50), lwd=1.1) +
    geom_path( aes(y=q.50.full), lwd=.9, lty=3) +
    geom_path( aes(y=q.50.avg), lwd=.9, lty=2, color='blue') +
    geom_ribbon( aes(ymin=q.05, ymax=q.95 ), alpha=.25 ) +
    geom_hline(aes(yintercept=0), lwd=.5) +
    facet_wrap( ~outcome.lab, scales='free_y' ) + # , label=labeller( outcome.f=var.labs )) +
    theme_minimal() +
    ylab('') +
    xlab(xlab)
  
  return(gg.out)
}

if( exists('df.robust.end') & robust){
  df.full <- df.robust.end %>%
    group_by(outcome) %>% 
    summarise( q.50.full=q.50[final.date==max(final.date)] )
  gg.robust.end <- make.roll.chart( df.robust.end, 'final.date', 'Final date', df.full )
  ggsave( paste0( 'graphs/', figure.location, '/roll_end_', l.var.coefs$lags,
                  '_lags.pdf'), gg.robust.end, height = 8, width = 10)
  # gg.robust.start <- make.roll.chart( df.robust.start, 'init.date', 'Initial date', df.full )
  # ggsave( paste0( 'graphs/', fcast.var.name, '_irf_var_decomp_', l.var.coefs$lags,
  #                 '_lags_roll_start.pdf'), gg.robust.start, height = 8, width = 10)
  # gg.robust.roll <- make.roll.chart( df.robust.roll, 'final.date', 'Final date', df.full )
  # ggsave( paste0( 'graphs/', figure.location, '/roll_both_', l.var.coefs$lags,
  #                 '_lags.pdf'), gg.robust.roll, height = 8, width = 10)
}


#### 6. The Machine-Learning VARs ####
l.outcomes$base <- c(fcast, x, paste0( x, '.sentiment'), paste0( x, '.fire'), 'fed.funds.rate', 'log.ip' )
  # Redefine for prettiness
if(exists('df.big.irf.struct.full') & robust){
  for( this.fcast in 1:n.fcast ){
    df.big.irf.nf <- df.big.irf.struct.full %>% filter(str_detect(shock, paste0('Sentiment #', this.fcast))) %>%
      mutate( outcome.lab=var.labs[outcome],
              value=ifelse(outcome %in% annualize.vars, annualize(value), value) ) %>%
      full_join( df.irf.nf %>% mutate(var.method=' Baseline') )
    for(this.outcomes in names(l.outcomes)){
      df.big.irf.nf <- df.big.irf.nf %>%
        mutate( outcome.f = fct_relevel( outcome.f, l.outcomes[[this.outcomes]] ) ) 
      gg.big.irfs.struct.nf <- 
        ggplot( df.big.irf.nf %>% filter( outcome %in% l.outcomes[[this.outcomes]] ),
                aes(x=period, y=value, group=var.method, color=var.method, shape=var.method ) ) +
        geom_path(lwd=.9) +
        geom_point(size=2) +
        geom_hline(aes(yintercept=0), lwd=.5) +
        facet_wrap( ~outcome.lab, scales='free_y' ) + #, label=labeller( outcome.f=var.labs )) +
        theme_minimal() + # ylim(irf.ylim) +
        xlab('Horizon') + ylab('') +
        scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) ) +
        theme(legend.position = 'bottom') +
        labs(color='Specification', shape='Specification')
      gg.big.irfs.struct.nf %>% print
      ggsave( paste0( 'graphs/', figure.location, '/irf_struct_nf_', l.var.coefs$lags,
                      '_lags_ML_', this.outcomes, '_', this.fcast, '_shk.pdf'), gg.big.irfs.struct.nf, height = 6, width = 10)
    }
  }
}

if(exists('df.favar.irf.struct.full') & robust){
  
  for( this.fcast in 1:n.fcast ){
    df.favar.irf.nf <- df.favar.irf.struct.full %>% 
      filter(str_detect(shock, paste0('Sentiment #', this.fcast))) %>%
      mutate( outcome.lab=var.labs[outcome],
              value=ifelse(outcome %in% annualize.vars, annualize(value), value) ) %>%
      full_join( df.irf.nf %>% mutate(var.method='Baseline') ) %>%
      filter( !str_detect( outcome, 'PC' ) ) %>%
      mutate( outcome.f = fct_relevel( outcome.f, l.outcomes$base ),
              var.method = fct_relevel( var.method, c( 'Baseline', paste0( n.favar, ' component FAVAR' ) ) ) ) 
    gg.favar.irfs.struct.nf <- 
        ggplot( df.favar.irf.nf %>% filter( outcome %in% l.outcomes$base ),
                aes(x=period, y=value, group=var.method, color=var.method, shape=var.method ) ) +
        geom_path(lwd=.9) +
        geom_point(size=2) +
        geom_hline(aes(yintercept=0), lwd=.5) +
        facet_wrap( ~outcome.lab, scales='free_y') +
        theme_minimal() + # ylim(irf.ylim) +
        xlab('Horizon') + ylab('') +
        scale_x_continuous( breaks=seq(0,irf.plot.pds,6), limits = c(0,irf.plot.pds) ) +
        theme(legend.position = 'bottom') +
        labs(color='Specification', shape='Specification')
    gg.favar.irfs.struct.nf %>% print
    ggsave( paste0( 'graphs/', figure.location, '/irf_struct_nf_', l.var.coefs$lags,
                      '_lags_FAVAR_', this.fcast, '_shk.pdf'), gg.favar.irfs.struct.nf, height = 6, width = 10)
  
    df.favar.boot.full <- est.bootstrap$structural %>%
      filter(str_detect(shock, paste0('Sentiment #', this.fcast)), period==0) %>%
      mutate( outcome.lab=var.labs[outcome] ) %>%
      select(-period) %>%
      ungroup() %>%
      mutate( var.method='Baseline' ) %>%
      select( outcome, var.method, everything(), -shock ) %>%
      full_join( df.favar.boot ) %>%
      mutate(outcome.lab=var.labs[outcome],
             var.method = fct_relevel( var.method, c( 'Baseline', paste0( n.favar, ' component FAVAR' ) ) ) )
    gg.favar.sig.struct.nf <-
        ggplot( df.favar.boot.full %>% filter( outcome %in% l.outcomes$base ),
                aes(x=var.method, group=var.method, color=var.method, shape=var.method ) ) +
        geom_point(size=2, aes(y=q.50)) +
        geom_errorbar(aes(ymin = q.05, ymax = q.95), width = 0.1) +
        geom_hline(aes(yintercept=0), lwd=.5) +
        facet_wrap( ~outcome.lab, scales='free_y') +
        theme_minimal() + # ylim(irf.ylim) +
        xlab('') + ylab('') +
        theme(legend.position = 'bottom') +
        labs(color='Specification', shape='Specification') +
        theme(axis.text.x = element_blank())
    gg.favar.sig.struct.nf %>% print
    ggsave( paste0( 'graphs/', figure.location, '/sig_struct_nf_', l.var.coefs$lags,
                      '_lags_FAVAR.pdf'), gg.favar.sig.struct.nf, height = 6, width = 10)
  }
}
