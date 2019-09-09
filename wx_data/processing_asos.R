library(lattice)
library(tidyverse)
library(rNOMADS)
library(sp)
library(raster)

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/wx_data/asos_april-july_2018.csv'
col_names = c('station', 'valid_utc', 'lon', 'lat', 'tmpc', 'dwptc', 'relh',
              'drctn', 'spd', 'precip_1hr_mm', 'vis', 'gust_mph', 'wxcodes')
asos <- read.csv(fpath, header=TRUE, stringsAsFactors = FALSE, col.names=col_names,
                 na.strings=c('M', '#NAME?', 'T'))

tslice <- asos[asos$valid_utc == '4/1/2018 0:00',]

coordinates(asos) <- ~lon+lat # create the spatial points df from asos data
rast <- raster(ncol=10, nrow=10)
extent(rast) <- extent(asos)

# below 
plot(rasterize(asos, rast, asos$vis, fun=mean))
