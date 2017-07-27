# 02_PBS2015_modP2_expenditures_clean.R ------------------------------------------
# Bangladesh analysis 2.0; clean medical expenditures
# Code to clean up the expenditure module from the Bangladesh Integrated Household Survey, 2015
# Data downloaded 14 June 2017 from Harvard's Dataverse: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BXSYEL
# USAID | GeoCenter
# Laura Hughes, lhughes@usaid.gov, 25 July 2017



# general procedure --------------------------------------------------------
# 1. Import data, rename to meaningful variables
# 2. Filter out the medical expenses; classify as male or female expenditures.
# Roll up to the household level, summarising cost of male expenses and female.
# Merge w/ hh base

# KNOWN ISSUES ------------------------------------------------------------


# import data -------------------------------------------------------------
exp_raw = read_dta(paste0(pbs_dir, '046_r2_mod_p2_male.dta'))
'~/Documents/Bangladesh/rawdata/BGD_2015_PBS/046_r2_mod_p2_male.dta'

# expenditure codes
male_medical = 117:127
female_medical = 128:139

# clean expenditures ------------------------------------------------------------

expend = exp_raw %>% 
  mutate(
    exp_code = as.numeric(p2_01),
    exp_descrip = p2_01, # copy
    # identify if male or female medical expense
    exp_type = ifelse(exp_code %in% male_medical, 'male_medical', 
                      ifelse(exp_code %in% female_medical, 'female_medical', 0)),
    exp_cost = p2_03) %>% 
  # filter out just medical expenses
  filter(exp_type != 0) %>% 
  # convert expense type to factor
  mutate(exp_descrip2 = plyr::mapvalues(as.numeric(exp_descrip), from = dict$level, to = dict$label)) %>% 
  select(a01, exp_code, exp_descrip, exp_descrip2, exp_cost, exp_type)


# characterize most common (and costly) type of medical expenses -----------------------
expend %>% 
  group_by(exp_type) %>% 
  count(exp_code) %>% 
  arrange(desc(n))

ggplot(expend, aes(x = fct_reorder(as.factor(exp_code), exp_cost), y = exp_cost, fill = exp_type)) +
  geom_violin() +
  coord_flip() +
  scale_y_log10() +
  theme_minimal()

# collapse to the hh-level ------------------------------------------------
exp_hh = expend %>% 
  group_by(a01, exp_type) %>% 
  summarise(exp_tot = sum(exp_cost)) %>% 
  spread(exp_type, exp_tot)

full_join(exp_hh, shk, by = 'a01') %>% 
  # replace no medical shock w/ 0's
  mutate(shock = coalesce(shock, 0),
         # replace no medical expenses w/ 0's
         # assume if no expenses listed, must be 0 taka.
         male_medical = coalesce(male_medical, 0),
         female_medical = coalesce(female_medical, 0)
  ) %>% 
  group_by(shock) %>% 
  summarise(m = mean(male_medical),
            f = mean(female_medical),
            shk = mean(shk_cost),
            n = n())
