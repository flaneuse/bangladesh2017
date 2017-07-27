# 01_PBS2015_modA_hhbase_clean.R ------------------------------------------
# Bangladesh analysis 2.0; clean household base characteristics
# Code to clean up the household base module from the Bangladesh Integrated Household Survey, 2015
# Data downloaded 14 June 2017 from Harvard's Dataverse: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BXSYEL
# USAID | GeoCenter
# Laura Hughes, lhughes@usaid.gov, 25 July 2017



# import data -------------------------------------------------------------
hh = read_dta(paste0(pbs_dir, '001_r2_mod_a_male.dta'))


# Clean up vars -----------------------------------------------------------

hh = hh %>% 
  # remove hh respondent, hh father id
  select(-a10, -a12) %>% 
  # rename vars
  rename(head_id = a11,
         religion = a13,
         language = a14,
         ethnic = a15) %>% 
  # factorize
  # Note: hh_type classifies hh into FTF or non-FTF; however, there were 2 stages of FTF hh identification:
  # https://feedthefuture.gov/sites/default/files/resource/files/Bangladesh_Feed_the_Future_Baseline_Country_Report_English.pdf
  lab2factor('hh_type') %>% 
  lab2factor('religion') %>% 
  lab2factor('language') %>% 
  lab2factor('ethnic') %>% 
  # create copy
  mutate(hh_id = a01,
         ftf_flag = ifelse(hh_type %like% 'Feed', 1, 0)) %>% 
  # filter only those hh that responded to entire survey
  filter(flag_a == 1)
