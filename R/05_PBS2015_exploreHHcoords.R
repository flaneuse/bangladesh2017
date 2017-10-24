library(sp)
library(sf)
library(leaflet)
library(haven)
library(tidyverse)
library(llamar)
library(geocenter)

degMinSec2decimal = function (deg, min, sec) {
  degrees = as.numeric(deg)
  minutes = as.numeric(min)
  seconds = as.numeric(sec)
  
  if(degrees < 0){
    return(degrees - minutes/60 - seconds/3600)
  } else {
    return(degrees + minutes/60 + seconds/3600)
  }
}

degMin2decimal = function (deg, min, min_thousandths = NA) {
  degrees = as.numeric(deg)
  minutes = as.numeric(min)
  
  if(!is.na(min_thousandths)){
    thousandths = as.numeric(min_thousandths)
    
    minutes = minutes + thousandths/1000
  }
  
  if(degrees < 0){
    return(degrees - minutes/60)
  } else {
    return(degrees + minutes/60)
  }
}

read_geo = function(file){
  geo = read_dta(file) %>% 
    separate(a08_e, into = c('lon_deg', 'lon_min', 'lon_mindecimal'), sep = '\\.', remove = FALSE) %>% 
    separate(a08_n, into = c('lat_deg', 'lat_min', 'lat_mindecimal'), sep = '\\.', remove = FALSE) %>% 
    mutate(lon = degMin2decimal(lon_deg, lon_min, lon_mindecimal),
           lat = degMin2decimal(lat_deg, lat_min, lat_mindecimal),
           supervisor = as.character(a19),
           enumerator = as.character(a18)) %>% 
    filter(!is.na(lat), !is.na(lon))
  
  # convert to spatial object
  sp::coordinates(geo) = ~lon+lat
  geo = st_as_sf(geo)
  st_crs(geo) = '+proj=longlat +datum=WGS84 +no_defs'
  
  return(geo)
}

geo2 = read_geo('~/Documents/Bangladesh/rawdata/BGD_2015_PBS/SBU_r2_male_mod_a_001.dta')
geo1 = read_geo('~/Documents/Bangladesh/rawdata/BGD_2011_PBS/SBU_baseline_male_mod_a_005.dta') %>% st_transform('+proj=laea +lat_0=23.751043336315906 +lon_0=90.37902832031249')
tim = read_dta('~/Documents/Bangladesh/rawdata/BGD_2011_PBS/Geovariables.dta')

# define categorical color palette
colPal = leaflet::colorFactor(palette = c(category20, 'black'), levels = unique(geo$supervisor))
colPal = leaflet::colorFactor(palette = rep('orange', 40), levels = unique(geo$supervisor))

# create leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircles(data = geo2, color = ~colPal(supervisor),
             radius = 500, fillOpacity = 0.4, stroke = FALSE) 
