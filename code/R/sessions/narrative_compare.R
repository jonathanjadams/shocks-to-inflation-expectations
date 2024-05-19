##########################################################################################
#
# narrative_compare.R
#
# Code to make charts comparing the shocks to those identified by narrative means
# Philip Barrett, Washington DC
# First version: 10oct2023
#
##########################################################################################

#### 0. Set up ####
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(zoo)
library(xtable)
select <- dplyr::select
# Otherwise picks up the MASS usage 

#### 1. Controls ####
results.file <- 'model_solutions/michigan_cpi_1982_m_3.rdata'
# results.file <- 'data/fed_1982_q_4.rdata'
  # The locations of the data
results.name <- results.file %>% gsub('model_solutions/', '', .) %>%
  gsub('.rdata', '', .)
factiva.file <- 'data/factiva_search/inflation_factiva.csv'
fomc.file <- 'data/factiva_search/aruoba_data.csv'


#### 2. Assemble the data ####

## 2.1 The estimated residuals ##
load( results.file )


struct.resid <- solve( est.A$A, l.var.coefs$var %>% resid() %>% t ) %>% t
df.struct.resid <- data.frame( date = df$date %>% unique() %>% sort, 
                               rbind( matrix( NA, nrow=l.var.coefs$lags, ncol=est.A$A%>%ncol ),
                                      struct.resid ) )
names(df.struct.resid) <- c( 'date', 'Non-fundamental', 
                             paste0( 'Non-fundamental #', 1:(ncol(df.struct.resid)-2) ) )
df.struct.resid.long <- df.struct.resid %>% 
  gather(shock, value, -date) %>% 
  group_by(shock) %>%
  mutate( value.12m=rollmean(value, 12, fill=NA, align='r' ) )

gg.sentiment <- ggplot( df.struct.resid.long %>% 
                          # filter( date >= '2020-01-01') %>%
                          filter( shock=='Non-fundamental') %>%
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
  ylab('Inflation sentiment') +
  theme( legend.position = 'bottom' ) +
  labs(color=NULL)
    # Reproduce the sentiment time series plot

## 2.2 The media searches ##
df.factiva.in <- read_csv(factiva.file) %>%
  rename(date=Label) %>%
  mutate( date=as.Date(date, format='%d-%b-%y') )
x <- 1:nrow(df.factiva.in)
df.factiva.res <- df.factiva.in %>% 
  mutate_if( is.numeric,
  function(y) lm( y ~ poly(x,2) ) %>% residuals ) %>%
  mutate_if( is.numeric,
             function(y) y / sd(y) ) %>%
  gather( variable, value, -date ) %>%
  mutate( variable=gsub('_inflation', '', variable) ) %>%
  group_by(variable) %>%
  mutate( value.12m=rollmean(value, k=12, fill=NA, align='r' ) )

ggplot( df.factiva.res, aes(x=date, y=value, group=variable, color=variable ) ) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = 'bottom')

ggplot( df.factiva.res, aes(x=date, y=value.12m, group=variable, color=variable ) ) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = 'bottom')

df.factiva.avg <- df.factiva.res %>% 
  group_by(date) %>% summarise_if( is.numeric, mean ) %>%
  mutate( variable='Five newspaper average') %>%
  full_join( df.factiva.res %>% filter(variable=='WSJ') %>%
               mutate(variable = ' WSJ') )

## 2.3 FOMC sentiments ##
df.fomc.in <- read_csv(fomc.file) %>%
  rename(date=Label) %>%
  mutate( date=as.Date(date, format='%d-%b-%y') )
x <- 1:nrow(df.fomc.in)
df.fomc.res <- df.fomc.in %>% 
  mutate_if( is.numeric,
             function(y) lm( y ~ poly(x,2) ) %>% residuals ) %>%
  mutate_if( is.numeric,
             function(y) y / sd(y) ) %>%
  gather( variable, value, -date ) %>%
  mutate( variable=gsub('\\.', ' ', variable) %>% str_to_title ) %>%
  group_by(variable) %>%
  mutate( value.12m=rollmean(value, k=12, fill=NA, align='r' ) )

ggplot( df.fomc.res, aes(x=date, y=value, group=variable, color=variable ) ) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = 'bottom')

ggplot( df.fomc.res, aes(x=date, y=value.12m, group=variable, color=variable ) ) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = 'bottom')



#### 3. Merge and plot ####

## 3.1 Factiva searches
df.cht.narrative <- df.struct.resid.long %>%
  filter( shock=='Non-fundamental' ) %>%
  mutate(variable='Sentiment') %>%
  full_join( df.factiva.avg ) %>%
  filter( date <= max(df.struct.resid.long$date) ) %>%
  group_by( variable ) %>%
  mutate_if( is.numeric, function(x) x/sd(x, na.rm=T) )

ggplot( df.cht.narrative, aes(x=date, y=abs(value.12m), color=variable) )+
  geom_line() +
  theme_minimal() + 
  theme(legend.position = 'bottom')

ggplot( df.cht.narrative, aes(x=date, y=(value.12m), color=variable) )+
  geom_line() +
  theme_minimal() + 
  theme(legend.position = 'bottom')

ggplot( df.cht.narrative, aes(x=date, y=sign(value.12m), color=variable) )+
  geom_line() +
  theme_minimal() + 
  theme(legend.position = 'bottom')

df.cht.pos <- df.cht.narrative %>% select(-value, -shock) %>% 
  spread(variable, value.12m) %>% filter( Sentiment>0)
ggplot( df.cht.pos, aes(x=Sentiment, y=` WSJ`) )+
  geom_point() +
  theme_minimal() + 
  theme(legend.position = 'bottom')
# cov( df.cht.pos$Sentiment, df.cht.pos$` WSJ`, use='complete.obs')

tab.cor <- 
  df.cht.narrative %>% select(-value.12m, -shock) %>% 
    spread(variable, value) %>%
    filter(!is.na(Sentiment)) %>%
    mutate( sentiment.positive=ifelse(Sentiment>0, 'positive', 'negative') ) %>%
    group_by(sentiment.positive) %>%
    summarise( cor.WSJ=cor(Sentiment, ` WSJ`, use='complete.obs') %>% round(3),
               cor.WSJ.ci=paste0( '(', 
                      c(cor.test(Sentiment, ` WSJ`, use='complete.obs')$conf.int) %>% round(3)
                      %>% paste(.,collapse=', '), ')' ),
               cor.5.paper=cor(Sentiment, `Five newspaper average`, use='complete.obs' ) %>% round(3),
               cor.5.paper.ci=paste0( '(', c(cor.test(Sentiment, `Five newspaper average`, use='complete.obs')$conf.int) %>% round(3)
                                      %>% paste(.,collapse=', '), ')' ) ) %>%
  full_join( df.cht.narrative %>% select(-value.12m, -shock) %>% 
               spread(variable, value) %>%
               filter(!is.na(Sentiment))  %>%
               summarise( cor.WSJ=cor(Sentiment, ` WSJ`, use='complete.obs') %>% round(3),
                          cor.WSJ.ci=paste0( '(', 
                                             c(cor.test(Sentiment, ` WSJ`, use='complete.obs')$conf.int) %>% round(3)
                                             %>% paste(.,collapse=', '), ')' ),
                          cor.5.paper=cor(Sentiment, `Five newspaper average`, use='complete.obs' ) %>% round(3),
                          cor.5.paper.ci=paste0( '(', c(cor.test(Sentiment, `Five newspaper average`, use='complete.obs')$conf.int) %>% round(3)
                                                 %>% paste(.,collapse=', '), ')' ) ) %>%
               mutate( sentiment.positive=NA )  ) %>%
  full_join( df.cht.narrative %>% select(-value.12m, -shock) %>% 
               spread(variable, value) %>%
               filter(!is.na(Sentiment)) %>%
               summarise( cor.WSJ=cor(abs(Sentiment), ` WSJ`, use='complete.obs') %>% round(3),
                          cor.WSJ.ci=paste0( '(', 
                                             c(cor.test(abs(Sentiment), ` WSJ`, use='complete.obs')$conf.int) %>% round(3)
                                             %>% paste(.,collapse=', '), ')' ),
                          cor.5.paper=cor(Sentiment, `Five newspaper average`, use='complete.obs' ) %>% round(3),
                          cor.5.paper.ci=paste0( '(', c(cor.test(abs(Sentiment), `Five newspaper average`, use='complete.obs')$conf.int) %>% round(3)
                                                 %>% paste(.,collapse=', '), ')' ) ) %>%
               mutate( sentiment.positive='absolute' )  ) %>%
  gather( variable, value, -sentiment.positive ) %>%
  mutate(source=case_when( grepl('WSJ', variable)~' Wall Street Journal',
                           TRUE~'Five-newspaper average' ),
         type=case_when( grepl('ci', variable)~'CI',
                         TRUE~' Point est.' ),
         case=case_when(is.na(sentiment.positive)~' All observations',
                        sentiment.positive=='absolute'~'Absolute value of sentiment',
                        sentiment.positive=='positive'~'Sentiment>0', 
                        sentiment.positive=='negative'~'Sentiment<=0' ) ) %>%
  select(-variable) %>%
  spread(source, value) %>%
  arrange(case) %>%
  group_by(sentiment.positive) %>%
  mutate(case=c( case[1], rep('', n()-1) ) ) %>%
  ungroup() %>%
  select(-sentiment.positive, -type) %>%
  rename(' '=case)
print( xtable( tab.cor, label='t:wsj_correl',
       caption = 'Correlation and 95\\% confindence intervals of Sentiment shock and newspaper mentions of inflation', 
       align = c('l','l','c','c') ),
include.rownames=FALSE, hline.after=c(-1,-1,0,8), floating = FALSE, 
file = paste0( 'tables/wsj_correl_', results.name, '.tex' ) ) #,

## 3.2 FOMC sentiments
df.cht.fomc <- df.struct.resid.long %>%
  filter( shock=='Non-fundamental' ) %>%
  mutate(variable='Sentiment') %>%
  full_join( df.fomc.res ) %>%
  filter( date <= max(df.struct.resid.long$date) ) %>%
  group_by( variable ) %>%
  mutate_if( is.numeric, function(x) x/sd(x, na.rm=T) )

ggplot( df.cht.fomc, aes(x=date, y=value.12m, color=variable) )+
  geom_line() +
  theme_minimal() + 
  theme(legend.position = 'bottom')

ggplot( df.cht.fomc, aes(x=date, y=abs(value.12m), color=variable) )+
  geom_line() +
  theme_minimal() + 
  theme(legend.position = 'bottom')

df.cht.fomc.wide <- df.cht.fomc %>% select(-value, -shock) %>% 
  spread(variable, value.12m)
ggplot( df.cht.fomc.wide, aes(x=Sentiment, y=Inflation) )+
  geom_point() +
  theme_minimal() + 
  theme(legend.position = 'bottom')
ggplot( df.cht.fomc.wide, aes(x=Sentiment, y=`Inflation Expectations`) )+
  geom_point() +
  theme_minimal() + 
  theme(legend.position = 'bottom')
cor.fomc.inf <- cor.test( df.cht.fomc.wide$Sentiment, df.cht.fomc.wide$Inflation, use='complete.obs')
cor.fomc.exp <- cor.test( df.cht.fomc.wide$Sentiment, df.cht.fomc.wide$`Inflation Expectations`, use='complete.obs')

tab.cor.fomc <- sapply( list('FOMC Inflation'=cor.fomc.inf,
                             'FOMC Inflation Sentiments'=cor.fomc.exp ),
        function(x) c( x$estimate %>% round(3), 
                       paste0( '( ', paste0( round(x$conf.int,3), collapse=', ' ) , ')' ) ) ) %>%
  set_rownames(NULL) %>%
  as.data.frame()

print( xtable( tab.cor.fomc, label='t:fomc_correl',
               caption = 'Correlation and 95\\% confindence intervals of Sentiment shock and estimated FOMC sentiments',
               align = c('l','c','c') ),
       include.rownames=FALSE, hline.after=c(-1,-1,0,2), floating = FALSE, 
       file = paste0( 'tables/fomc_correl_', results.name, '.tex' ) ) #,


x
# df.cht.pos <- df.cht.fomc %>% select(-value, -shock) %>% 
#   spread(variable, value.12m) %>% filter( Sentiment>0)
# ggplot( df.cht.pos, aes(x=Sentiment, y=Inflation) )+
#   geom_point() +
#   theme_minimal() + 
#   theme(legend.position = 'bottom')
# ggplot( df.cht.pos, aes(x=Sentiment, y=`Inflation Expectations`) )+
#   geom_point() +
#   theme_minimal() + 
#   theme(legend.position = 'bottom')
# # cov( df.cht.pos$Sentiment, df.cht.pos$` WSJ`, use='complete.obs')
