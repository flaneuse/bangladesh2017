library(haven)


# set working directory ---------------------------------------------------

base_dir = '~/Documents/Bangladesh/rawdata/BGD_2014_DHS_SPA/'

df  = read_dta(paste0(base_dir, 'bdfc7adtsp/BDFC7AFLSP.DTA'))
