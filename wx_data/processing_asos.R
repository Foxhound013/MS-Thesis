library(lattice)
library(tidyverse)
library(rNOMADS)
library(sf)
library(tmap)

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/wx_data/asos_april-july_2018.csv'
col_names = c('station', 'valid_utc', 'lon', 'lat', 'tmpc', 'dwptc', 'relh',
              'drctn', 'spd', 'precip_1hr_mm', 'vis', 'gust_mph', 'wxcodes')
asos <- read.csv(fpath, header=TRUE, stringsAsFactors = FALSE, col.names=col_names,
                 na.strings=c('M', '#NAME?', 'T'))

tslice <- asos[asos$valid_utc == '4/1/2018 0:00',]
plot(tslice$vsby~tslice$lon)

#! Need to set the variable types and column names. A lot of things are getting sorted as factors
# when they really shouldn't be.

str(asos)
summary(asos)

#! Fill missing values with na

levelplot(tslice$vis~tslice$lat*tslice$lon)
levelplot(tslice$spd~tslice$lat*tslice$lon)

tslice2 <- asos[asos$valid_utc == '4/1/2018 10:55',]

levelplot(tslice$vis~tslice$lon*tslice$lat)
levelplot(tslice$spd~tslice$lon*tslice$lat)




asos_sf <- st_as_sf(asos, coords = c("lon", "lat"),
         crs = "+proj=longlat +datum=WGS84")

tslice <- asos_sf[asos$valid_utc == '4/1/2018 10:55',]
tslice

tslice2 <- asos_sf[,c('tmpc', 'geometry')]
plot(tslice2)

tm_dots(tslice2)


IN <- st_read('C:/Users/Downi/Documents/GitHub/MS-Thesis/wx_data/IN_shp')

tm_shape(IN) + tm
