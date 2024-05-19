##########################################################################################
#
# renaming.R
#
# Code to select and rename the charts and tables used in publication for ease of use
# Philip Barrett, Washington DC
# First version: 14may2024
#
##########################################################################################

#### 0. Set up ####


#### 1. Controls ####
l.prefix <- list( figs='graphs/replication/', tables='tables/' )
output.folder <- 'outputs/'

l.charts.to.rename <- 
  c( NA, 'michigan_cpi/irf_struct_nf_1_1982_m_3_lags.pdf',
     'michigan_cpi/sentiment_1_shk_3_lags.pdf',
     'michigan_cpi/sentiment_vs_UMCSENT_1_shk_3_lags.pdf',
     'compare/fcasts_irf_compare.pdf',              
          # Fig. 5
     'michigan_cpi/roll_end_3_lags.pdf',
     'michigan_cpi/irf_struct_nf_3_lags_FAVAR_1_shk.pdf',
     'michigan_pi_cpi/irf_struct_nf_2_1982_m_3_lags.pdf',
     'spf_tbill_gdp_pgdp/irf_struct_nf_3_1982_q_2_lags.pdf',
     'comparison_m.pdf',
          # Fig. 10
     'validation_range_m_nD.pdf',
     NA, NA,
     'michigan_cpi/var_decomp_3_lags.pdf',
     'compare/lags_irf_compare.pdf',
          # Fig. 15
     'michigan_cpi/irf_var_lp_1982_m_3_lags.pdf',
     'compare/lags_lp_compare.pdf',
     'michigan_cpi/sig_struct_nf_3_lags_FAVAR.pdf',
     'michigan_cpi/irf_struct_nf_3_lags_ML_base_1_shk.pdf',
     'compare/oil_irf_compare.pdf',
          # Fig. 20
     'michigan_cpi_oil_inf_2/irf_struct_fund_1_1982_m_3_lags.pdf',
     'michigan_cpi/impact_struct_nf_1_1982_m_3_lags_non_RE_show_ci.pdf',
     'compare/rnr_2_irf_compare.pdf',
     'compare/fcasts_fcast_compare.pdf',
     'michigan_cpi/time_series_m_int_rates.pdf',
          # Figure 25
     'michigan_cpi/time_series_m_financial.pdf',
     'michigan_cpi/time_series_m_real.pdf',
     'michigan_cpi/time_series_m_money_credit.pdf'
     )
l.tables.to.rename <- 
  c('fcasts_var_decomp.tex',
    'wsj_correl_michigan_cpi_1982_m_3.tex',
    'fomc_correl_michigan_cpi_1982_m_3.tex',
    NA,
    NA,
    'ml_eval.tex'
    )



#### 2. Main iteration ####
n.figs <- l.charts.to.rename %>% length
n.tables <- l.tables.to.rename %>% length

status <- list( figs=rep(NA,n.figs), tables=rep(NA,n.tables) )

for( i.fig in 1:n.figs ){
  this.fig <- paste0( l.prefix$figs, l.charts.to.rename[i.fig] )
    # The current figure
  if( is.na(l.charts.to.rename[i.fig]) ){
    status$figs[i.fig] <- 'Figure not created here'
  }else{
    this.file.exists <- file.exists( this.fig )
      # Does the file exist
    if(this.file.exists){
      file.copy( this.fig, paste0(output.folder, 'figure_', i.fig, '.pdf'), overwrite = TRUE)
        # Copy and rename the figure as needed
      status$figs[i.fig] <- 'OK'
    }else{
      status$figs[i.fig] <- 'Error'
    }
  }
}

for( i.tab in 1:n.tables ){
  this.tab <- paste0( l.prefix$tables, l.tables.to.rename[i.tab] )
      # The current table
  if( is.na(l.tables.to.rename[i.tab]) ){
    status$tables[i.tab] <- 'Table not created here'
  }else{
    this.file.exists <- file.exists( this.tab )
        # Does the file exist
    if(this.file.exists){
      file.copy( this.tab, paste0(output.folder, 'table_', i.tab, '.tex'), overwrite = TRUE )
          # Copy and rename the table as needed
      status$tables[i.tab] <- 'OK'
    }else{
      status$tables[i.tab] <- 'Error'
    }
  }
}

message(" **** Status of final chart ****")
print(status)






