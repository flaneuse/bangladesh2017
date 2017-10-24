# 04_PBS2015_merge_locations.R ------------------------------------------
# Bangladesh analysis 2.0; merge lat/lon 
# Code to merge in the geographic coordinates of households to the Bangladesh Integrated Household Survey, 2015
# 
# Household locations were provided by IFPRI 
# USAID | GeoCenter
# Laura Hughes, lhughes@usaid.gov, 1 August 2017

# Identify hh that have split off: 208 hh in 2015
glimpse(hh %>% filter((a01*10) %% 2 != 0) %>% count(a01))

hh = hh %>% 
  mutate(div = as.integer(div),
         District = as.numeric(District),
         Upazila = as.numeric(Upazila),
         Union = as.numeric(Union))
  

geo_base = read.csv('~/Documents/Bangladesh/processeddata/BGD_2011_PBS/SBU_BGD_latlonmapped.csv', stringsAsFactors = FALSE) %>% 
  mutate(a01 = as.numeric(a01))


# merge together fields ---------------------------------------------------

# "a01": hh id; essential
# "a02": census number; only partially filled out in 2015.  dropping.
# "hh_type": FTF or not; needs to be string case unified
# "div": convert 2015 to integer
# "div_name": maybe okay?
# "dcode": 2015: need to convert to a char from labelled (and string cased)
# "uzcode": 2015: need to convert to a char from labelled (and string cased)
# "uncode":  2015: need to convert to a char from labelled (and string cased)
# "mzcode": partially missing in 2011? drop
# "mouzacode": partially missing in 2011? drop
# "mouza_name": partially missing in 2011? drop
# "vcode": 2015: need to convert to a char from labelled (and string cased)
# "village_name": needs to have special characters removed

geo_base %>% count(is.na(latitude))
# 
# test = left_join(hh, geo_base, by = c("a01", "hh_type", "div", "div_name",
# "dcode", "uzcode", "uncode", "mzcode",  "mouzacode", 
# "mouza_name", "vcode", "village_name")

x = hh %>% slice(1:10) %>% select_("a01", "a02", "hh_type", "div", "div_name",
                                     "dcode", "uzcode", "uncode", "mzcode",  "mouzacode", 
                                     "mouza_name", "vcode", "village_name")
y = geo_base %>% slice(1:10) %>% select_("a01", "a02", "hh_type", "div", "div_name",
                                   "dcode", "uzcode", "uncode", "mzcode",  "mouzacode", 
                                   "mouza_name", "vcode", "village_name")
