
# Setup file to establish libraries, constants, directories, etc. ---------
# Bangladesh analysis 2.0
# USAID | GeoCenter
# Laura Hughes, lhughes@usaid.gov, 25 July 2017

# working directories -----------------------------------------------------
# raw data directory
import_dir = '~/Documents/Bangladesh/rawdata/'
pbs_dir = paste0(import_dir, 'BGD_2015_PBS/')
gh_dir = '~/Documents/GitHub/bangladesh2017/'
export_dir = paste0(gh_dir, 'exportedfromR/')

# libraries ---------------------------------------------------------------
library(dplyr) # version 0.7.1
library(data.table) # version 1.10.4
library(haven) # version 1.0.0
library(tidyr) # version 0.6.3
library(forcats) # version 0.2.0
library(ggplot2) # version 2.2.1.9000
library(sf) # version 0.5-1
library(stringr) # version 1.2.0
library(readxl) # version 1.1.0


# generic function to convert labeled object to factor --------------------
lab2factor = function(data, colname, ordered = FALSE) {
  dict = data.frame(level = attr(data[[colname]], 'labels'),
                    label = names(attr(data[[colname]], 'labels')))
  
  data[[colname]] =  factor(data[[colname]],
                           levels = dict$level, 
                           labels = dict$label,
                           ordered = ordered)
  
  return(data)
}

lab2char = function(data, colname, ordered = FALSE) {
  dict = data.frame(level = attr(data[[colname]], 'labels'),
                    label = names(attr(data[[colname]], 'labels')))
  
  data[[colname]] = as.character(plyr::mapvalues(data[[colname]], from = dict$level, to = dict$label))
  
  return(data)
}


# clean up stringed data --------------------------------------------------
clean_string = function(string) {
  str_trim(str_to_lower(str_replace_all(string, '  ', ' ')))
}

