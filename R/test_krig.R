# test kriging

# tim_raw = read_dta('~/Documents/Bangladesh/rawdata/BGD_2011_PBS/Geovariables.dta')
# 
# ndigits = 2
# tim = tim_raw %>% 
#   mutate(lon = round(longitude, ndigits), lat = round(latitude, ndigits)) %>% 
#   group_by(lon, lat) %>% 
#   summarise_if(is.numeric, funs(mean)) %>% 
#   ungroup() %>% 
#   sample_n(100)

tim = read.csv('~/Desktop/test.csv') %>% 
  ungroup() %>% 
  group_by(lon = X, lat = Y) %>%
  summarise_if(is.numeric, funs(mean)) %>% 
  ungroup() %>% 
  sample_n(200)

geodata = list(coords = data.frame(x = tim$lon, y = tim$lat), data = tim$a01)

x_lim = range(geodata$coords$x)
y_lim = range(geodata$coords$y)

# by = max(abs(diff(x_lim)), abs(diff(y_lim))) / 20

# esri
by = min(abs(diff(x_lim)), abs(diff(y_lim)))/250
geo_grid = as.matrix(expand.grid(seq(x_lim[1] - by, x_lim[2] + by, by = by), seq(y_lim[1] - by, y_lim[2] + by, by = by)))
library(geoR)

x = krige.bayes(geodata, locations = geo_grid, prior = prior.control(phi.prior = 'exponential', phi = 2.5))

microbenchmark(krige.bayes(geodata, locations = geo_grid), times = 1)
image(x, main = 'predicted values')
image(x, val="variance", main="prediction variance")
