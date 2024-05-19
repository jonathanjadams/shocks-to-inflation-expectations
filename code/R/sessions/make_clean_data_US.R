##########################################################################################
#
# make_clean_data_US.R
#
# Code to prepare a dataset for the "Inflation Sentiments" project
# Philip Barrett, Washington DC
# First version: 25jan2022
#
##########################################################################################

#### 0. Set up ####
library(tidyverse)
library(readxl)
library(forecast)
library(ggplot2)
library(lubridate)

### 0.1 Function definitions ###
diff.pad.100 <- function(x,lag=1, fun=identity) c( rep(NA, lag), diff(fun(x),lag) ) * 100
    # Padded difference function
deseas.lm <- function(value, date, mon) lm( value ~ 1 + poly(as.numeric(date), 2) + mon ) %>% residuals()
deseas.lm.mod <- function(value, date, mon) lm( value ~ 1 + poly(as.numeric(date), 2) + mon )
# deseas.lm <- function(value, date, mon) lm( value ~ 1 + as.numeric(date) + mon ) %>% residuals()
    # Deseasonalizing by linear regression on month dummies
select <- dplyr::select
    # Otherwise picks up the MASS usage

#### 1. Controls ####
xl.in.dta <- 'data/Baseline Time Series/inflation_sentiments_update.xls'
xl.in.sheet <- list( m='Monthly', q='Quarterly', d.fin='Daily', d.idx='Daily,_Close', wk.fri='Weekly,_Ending_Friday',
                     wk.sat='Weekly,_Ending_Saturday', wk.wed='Weekly,_Ending_Wednesday' )
xl.in.michigan.income <- 'data/MSC_expected_income_increase_percent.csv'
xl.in.fed <- 'data/Greenbook Forecasts/GBweb_Row_Format_update.xlsx'
xl.in.fed.sheet <- 'gPCPI'
xl.spf.inflation <- 'data/SPF/Inflation_update.xlsx'
xl.spf.others <- 'data/SPF/meanLevel_updated.xlsx'
xl.var.rename <- list( m=c( 'michigan.fcast'='MICH', 'cleveland.fcast'='EXPINF1YR', 'cpi'='CPIAUCSL',
                            'pce'='PCE', 'commod.ppi'='PPIACO', 'unemp'='UNRATE',
                            'personal.income'='PI',
                            'housing.starts'='HOUST', 'm1'='M1SL', 'm2'='M2NS', 'currency'='MBCURRCIR',
                            'g.surp'='MTSDS133FMS', 'michigan.sentiment'='UMCSENT',
                            'emp'='USPRIV', 'p.oil'='WTISPLC', 'wk.hrs'='AWHMAN', 'vehicles'='TOTALSA',
                            'fed.funds.rate'='FEDFUNDS', 'ind.prod'='INDPRO', 'date'='DATE',
                            'inf.cpi.first'='CPI_1M_INF_FIRST'),
                       q=c( 'rgdp'='GDPC1', 'p.gdp'='GDPDEF', 'inv'='GPDIC1', 
                            'cons.gdp'='DPCERE1Q156NBEA', fed.funds.rate='FEDFUNDS', tbill='DTB3'),
                       d.fin=c( 'fx.cad'='DEXCAUS', 'fx.jpn'='DEXJPUS', 'fx.mex'='DEXMXUS', 'fx.gbr'='DEXUSUK',
                                'i.1.mo'='DGS1MO','i.3.mo'='DGS3MO', 'i.1.yr'='DGS1', 'i.2.yr'='DGS2',
                                'i.5.yr'='DGS5', 'i.10.yr'='DGS10', 'i.30.yr'='DGS30' ), #'i.20.yr'='DGS20', 
                       d.idx=c( 'will.idx'='WILL5000INDFC' ), wk.fri=c( 'leverage.idx'='NFCILEVERAGE'),
                       wk.sat=c( 'initial.claims'='ICSA'), wk.wed=c( 'bank.credit'='TOTBKCR') )
drop.vars <- list( m=c( 'cpi', 'commod.ppi', 'michigan.sentiment', 'm2', 'ind.prod', 'emp', 'p.oil',
                        'fx.cad', 'fx.jpn', 'fx.mex', 'fx.gbr', 'will.idx', 'bank.credit', 'm1', 'i.1.mo', 'DGS20',
                        'initial.claims', 'currency', 'g.surp', 'housing.starts', 'p.oil.r', 'vehicles' ),
                            # The Mexican exch rate starts in 1993 and the one-month rate in 2001
                   q=xl.var.rename$q )
  # Variables to drop (non-stationary)
out.file <- 'data/baseline_time_series_update.rdta'


#### 2. Read in process the monthly data ####
make.mly.avg <- function(df, v.rename) df %>% 
  mutate(year=year(DATE), month=month(DATE)) %>%
  group_by(year, month) %>% summarise_all(mean, na.rm=TRUE) %>%
  mutate( date=make_date(year, month, 1)) %>% ungroup() %>%
  select( date, everything(), -DATE, -year, -month ) %>%
  rename(v.rename)
df.m.agg <- lapply( c('d.fin', 'd.idx', 'wk.fri', 'wk.sat', 'wk.wed'), 
                    function(x) read_xls(xl.in.dta, sheet = xl.in.sheet[[x]], guess_max = 10000 ) %>% make.mly.avg( v.rename=xl.var.rename[[x]] ) ) %>%
  reduce(full_join)

df.m.in <- read_xls(xl.in.dta, sheet = xl.in.sheet$m )
df.income.fcast.in <- read_csv(xl.in.michigan.income) 
    # Read in the data
df.income.fcast <- df.income.fcast.in %>%
  rename( gth.fcast=Median ) %>%
  mutate( date=make_date(Year, Month, 1) ) %>%
  select( date, gth.fcast )
  # Set up the income forecast

df.m <- df.m.in %>%
  rename(xl.var.rename$m) %>%
  arrange(date) %>%
  full_join( df.m.agg ) %>%
  full_join( df.income.fcast ) %>%
  mutate( inf.cpi=diff.pad.100(cpi, fun=log), 
          inf.commod=diff.pad.100(commod.ppi, fun=log), 
          log.m1=100*log(m1), log.m2=100*log(m2), 
          log.currency=100*log(currency),
          log.initial.claims=100*log(initial.claims),
          log.fx.cad=diff.pad.100(fx.cad, fun=log), log.fx.jpn=log(fx.jpn), log.fx.gbr=log(fx.gbr),
          log.housing.starts=log(housing.starts), g.surp.idx=g.surp/ind.prod /sd(g.surp/ind.prod, na.rm=TRUE ),
          log.will.idx=log(will.idx), log.bank.credit=log(bank.credit), 
          log.emp=log(emp), log.vehicles=log(vehicles), log.wk.hrs=log(wk.hrs),
          log.p.oil=100*log(p.oil),
          inf.oil=diff.pad.100(p.oil, fun=log), 
          p.oil.r=p.oil / cpi, log.p.oil.r=100*log(p.oil.r),
          # unemp.diff=diff.pad.100(unemp, fun=identity)/100,
          # fed.funds.diff=diff.pad.100(fed.funds.rate, fun=identity)/100, 
          log.ip=100*log(ind.prod),
          log.real.pi= 100 * (log(personal.income)-log(cpi)),
          log.real.pi.fcast = log.real.pi + 100*log( (1+.01*(gth.fcast-michigan.fcast)) ),
          mon= date %>% month %>% as.factor ) %>%
  select( -(drop.vars$m) ) %>%
  na.omit() %>%
  select( date, mon, everything() ) %>%
  gather( variable, value, -date, -mon ) %>%
  group_by( variable ) %>%
  mutate( value.deseas=deseas.lm(value, date, mon) )
    # Process the data

# Make the inflation expectations with the same trend as CPI (i.e. do not detrend separately)
df.cpi <- df.m %>% filter(variable=='inf.cpi')
lm.deseas.cpi <- deseas.lm.mod(df.cpi$value, df.cpi$date, df.cpi$mon)
df.mich <- df.m %>% filter(variable=='michigan.fcast')
mich.trend <-  predict( lm.deseas.cpi, newdata = df.mich )
df.mich.common <- df.mich %>%
    mutate( variable='michigan.fcast.trend', 
            value.deseas=(value-mich.trend)-mean(value-mich.trend, na.rm=TRUE) )
df.m <- df.m %>% rbind(df.mich.common)

#### 3. Read in and process the quarterly data
df.q.in <- read_xls(xl.in.dta, sheet = xl.in.sheet$q )
  # Read in the data
df.q.spf.in <- read_xlsx(xl.spf.inflation, sheet='INFLATION')
df.q.spf <- df.q.spf.in %>%
  rename( year=YEAR, quarter=QUARTER, spf.fcast=INFPGDP1YR ) %>%
  mutate_all( function(x) ifelse(x=='#N/A',NA,x) ) %>%
  mutate(quarter=as.factor(quarter)) %>%
  select( year, quarter, spf.fcast ) %>%
  mutate(spf.fcast=spf.fcast %>% as.numeric())
  # The SPF inflation forecast
df.q.spf.irate.in <- read_xlsx(xl.spf.others, sheet='TBILL')
df.q.spf.irate <- df.q.spf.irate.in %>%
  rename( year=YEAR, quarter=QUARTER ) %>%
  mutate_all( function(x) ifelse(x=='#N/A',NA,x) %>% as.numeric() ) %>%
  mutate(quarter=as.factor(quarter)) %>%
  select( year, quarter, spf.fcast.irate=TBILL6 )
# The SPF inflation forecast
df.q.spf.ngdp.in <- read_xlsx(xl.spf.others, sheet='NGDP')
df.q.spf.ngdp.gth <- df.q.spf.ngdp.in %>%
  rename( year=YEAR, quarter=QUARTER ) %>%
  mutate_all( function(x) ifelse(x=='#N/A',NA,x) %>% as.numeric() ) %>%
  mutate( spf.fcast.ngdp.gth = 100*log( NGDP6 / NGDP2 ) ) %>%
  mutate(quarter=as.factor(quarter)) %>%
  select( year, quarter, spf.fcast.ngdp.gth )
    # The nominal GDP growth forecast
df.q.spf.pgdp.in <- read_xlsx(xl.spf.others, sheet='PGDP')
df.q.spf.pgdp.gth <- df.q.spf.pgdp.in %>%
  rename( year=YEAR, quarter=QUARTER ) %>%
  mutate_all( function(x) ifelse(x=='#N/A',NA,x) %>% as.numeric() ) %>%
  mutate( spf.fcast.pgdp.gth =100*log( PGDP6 / PGDP2 ) ) %>%
  mutate(quarter=as.factor(quarter)) %>%
  select( year, quarter, spf.fcast.pgdp.gth )
    # The nominal GDP growth forecast
df.q.spf.unemp.in <- read_xlsx(xl.spf.others, sheet='UNEMP')
df.q.spf.unemp <- df.q.spf.unemp.in %>%
  rename( year=YEAR, quarter=QUARTER ) %>%
  mutate_all( function(x) ifelse(x=='#N/A',NA,x) %>% as.numeric() ) %>%
  mutate(quarter=as.factor(quarter)) %>%
  select( year, quarter, spf.fcast.unemp=UNEMP6 )
  # The unemployment forecast one year ahead
df.q.spf.gdp.gth <- df.q.spf.ngdp.gth %>%
  full_join(df.q.spf.pgdp.gth) %>%
  mutate( spf.fcast.rgdp.gth = spf.fcast.ngdp.gth - spf.fcast.pgdp.gth)
    # The real GDP growth forecast
df.fed.in <- read_xlsx( xl.in.fed, xl.in.fed.sheet )
df.fed <- df.fed.in %>%
  separate( DATE, c('year', 'quarter'), sep='\\.') %>%
  mutate( date=ymd(GBdate) ) %>%
  select( date, year, quarter, matches('gPCPIF[1-4]') ) %>%
  na.omit() %>%
  group_by(year, quarter ) %>% 
  filter(row_number() >= n() ) %>%
    # ^^ Select the latest forecast
  mutate( fed.fcast=(gPCPIF1+gPCPIF2+gPCPIF3+gPCPIF4)/4,
          year=as.numeric(year), quarter=as.factor(quarter) ) %>%
  select( year, quarter, fed.fcast )
  # The Fed forecasts

df.q.agg <- df.m.in %>%
  select( date=DATE, unemp=UNRATE )
  #### USE LEFT_JOIN TO ADD ONLY THE END-OF-PERIOD UNEMPLOYMENT

df.q <- df.q.in %>%
  rename(xl.var.rename$q, date=DATE) %>%
  mutate(date=as.Date(date)) %>%
  arrange(date) %>%
  mutate( inf.gdp=diff.pad.100(p.gdp, fun=log),log.gdp=100*log(rgdp), 
          log.inv=100*log(inv), log.cons=100*log(cons.gdp/100*rgdp),
          year= date %>% year, quarter= date %>% quarter %>% as.factor ) %>%
  left_join(df.q.spf) %>%
  left_join(df.q.spf.gdp.gth) %>%
  left_join(df.q.spf.unemp) %>%
  left_join(df.q.spf.irate) %>%
  left_join( df.q.agg ) %>%
  mutate( spf.fcast.log.gdp = log.gdp + spf.fcast.rgdp.gth ) %>%
  left_join(df.fed) %>%
  select( date, year, quarter, everything() ) %>%
  gather( variable, value, -date, -quarter, -year ) %>%
  filter( !(variable %in% drop.vars$q) ) %>%
  na.omit() %>%
  group_by( variable ) %>%
  mutate( value.deseas=deseas.lm(value, date, quarter) ) %>%
  select(-year)
  # The quarterly data

  

#### 4. Save the data ####
save( df.m, df.q, file=out.file)




