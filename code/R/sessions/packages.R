##########################################################################################
#
# packages.R
#
# Installs any missing packages required for the inflation shocks project 
# Philip Barrett, Washington DC
# First version: 17may2024
#
##########################################################################################

required.packages <- c('tidyverse', 'xtable', 'magrittr', 'readxl', 'ggplot2',
                        'zoo', 'forecast', 'lubridate', 'BigVAR', 'expm', 'lpirfs',
                        'vars' )

installed.packages(required.packages)