# 02_PBS2015_modT_shocks_clean.R ------------------------------------------
# Bangladesh analysis 2.0; clean household shocks
# Code to clean up the household shock module from the Bangladesh Integrated Household Survey, 2015
# Data downloaded 14 June 2017 from Harvard's Dataverse: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BXSYEL
# USAID | GeoCenter
# Laura Hughes, lhughes@usaid.gov, 25 July 2017



# general procedure --------------------------------------------------------
# 1. Import data, rename to meaningful variables
# 2. Classify shocks into different types (ag, medical, etc.)
# 3. Roll up to the household level
# 4.  Merge w/ hh base (incl. lat/lon)


# ASSUMPTIONS -------------------------------------------------------
# • Include all shocks, or only most severe?
# • Inlude all shocks, or only ones after 2012?



# setup -------------------------------------------------------------------
source('00_setup.R')
source('01_PBS2015_modA_hhbase_clean.R')

# import data -------------------------------------------------------------
shk_raw = read_dta(paste0(pbs_dir, '050_r2_mod_t1_male.dta'))
'~/Documents/Bangladesh/rawdata/BGD_2015_PBS/046_r2_mod_p2_male.dta'

# shock cross-walk; converts from numeric code into consistent(ish) categories between
# these surveys and the LSMS surveys.
shk_codes = read_excel('~/Documents/GitHub/badsurprises/processeddata/shock_codes.xlsx') %>% 
  filter(country == 'Bangladesh', year == '2015',
         # only use negative shocks, not positive
         variable == 'T1_02') %>% 
  mutate(shk_id = as.numeric(shk_id)) %>% 
  select(-variable)

# [1] clean shocks ------------------------------------------------------------

shk = shk_raw %>% 
  # rename
  rename(shk_month = t1_04,
         shk_year = t1_05,
         # cost of shock, in taka
         shk_cost = t1_07,
         # primary coping mechanism
         shk_cope1 = t1_08a,
         # shock severity (1-3)
         shk_severity = t1_10) %>% 
  mutate(
    # create copies
    shk_id = as.numeric(t1_02),
    shk_descrip = t1_02,
    
    # convert to binary: no shock, shock since 2011, or NA (no response)
    shock = ifelse(shk_id == 99 & is.nan(t1_02a), 0, t1_02a),
    shk_repeated = ifelse(t1_03 > 1, 1, 0)
  ) %>% 
  
  # remove superfluous vars
  select(-hh_type, -contains('t1')) %>% 
  
  # convert labeled variables to factors
  lab2factor('shk_descrip') %>% 
  lab2factor('shk_cope1') %>% 
  
  # convert shock descriptions to uniform lowercased strings
  mutate(shk_descrip = clean_string(shk_descrip)) %>% 
  
  # [2] lump shocks into categories
  left_join(shk_codes, by = 'shk_id') %>% 
  # check if descriptions in imported data agree with the descriptions in the lookup table
  # Since Stata values truncated after 60 characters, only including 1st 60 chars for comparison
  mutate(descrip_match = str_sub(description, end = 60) == str_sub(shk_descrip, end = 60))

# Check merge between shock descriptions and lookup table matches
# All checked by eye; mismatches are due to encoding errors or more detail provided
shk %>% count(descrip_match)
View(shk %>% filter(descrip_match == FALSE) %>% select(shk_id, shk_descrip, description) %>% distinct() %>% arrange(shk_id))


# brief characterization --------------------------------------------------
qplot(data = shk, x = shk_year)


# [3] collapse to the hh-level ------------------------------------------------
# counted based on the number of categories of shocks
hh_shk = shk %>% 
  
  # filter out only shocks after 2012:
  filter(year > 2012) %>% 
  
  # group by hh id and the shock category
  group_by(a01, cause_cat) %>% 
  # count number of shocks
  summarise(num_shocks = sum(shock)) %>%
  # spread wide
  spread(cause_cat, num_shocks) %>% 
  # convert NAs to 0
  mutate(
    ag_shk = coalesce(agricultural, 0),
    crime_shk = coalesce(crime, 0),
    death_shk = coalesce(death, 0),
    fin_shk = coalesce(financial, 0),
    health_shk = coalesce(health, 0),
    other_shk = coalesce(other, 0)
  ) %>% 
  select(a01, contains ('shk'))


# [4] merge to hh base ----------------------------------------------------

hh = hh %>% 
  left_join(hh_shk, by = 'a01')


# Frequency of shocks -----------------------------------------------------
# quick bar graph of frequency of shock in a given hh

hh %>%
  select(a01, div_name, contains('shk')) %>% 
  gather(shk_cat, num_shocks, -a01, -div_name) %>% 
  mutate(shocked = num_shocks > 0) %>% 
  group_by(shk_cat, div_name) %>% 
  summarise(n_shk = sum(shocked),
            n = n(),
            pct_shk = n_shk/n) %>% 
  ggplot(., aes(y = pct_shk, x = fct_reorder(shk_cat, pct_shk),
                fill = shk_cat)) + 
  geom_bar(stat = 'identity') +
  
  scale_fill_brewer(palette = 'Pastel1', direction = -1) +
  scale_y_reverse(labels = scales::percent, name = 'percent of households experiencing a shock') +
  
  facet_wrap(~ div_name) +
  
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = 'none', 
        axis.title.y = element_blank())
