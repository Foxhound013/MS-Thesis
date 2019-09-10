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

# the above does rasterize your dataset.

##### Characterizing the ASOS Data #####

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/wx_data/asos_april-july_2018.csv'
col_names = c('station', 'valid_utc', 'lon', 'lat', 'tmpc', 'dwptc', 'relh',
              'drctn', 'spd', 'precip_1hr_mm', 'vis', 'gust_mph', 'wxcodes')
asos <- read.csv(fpath, header=TRUE, stringsAsFactors = FALSE, col.names=col_names,
                 na.strings=c('M', '#NAME?', 'T'))

asos$valid_utc <- as.POSIXct(asos$valid_utc, format="%m/%d/%Y %H:%M", tz="UTC")

# what is structure and summary of the data
str(asos)
summary(asos)

# How about missingness? How much of the data is missing for visibility?
vis.missing = (sum(is.na(asos$vis))/length(asos$vis)) * 100

# missingness of tmp?
tmpc.missing = (sum(is.na(asos$tmpc))/length(asos$tmpc)) * 100

# missingness of dpt?
dwptc.missing = (sum(is.na(asos$dwptc))/length(asos$dwptc)) * 100

# missingness of wind speed
spd.missing = (sum(is.na(asos$spd))/length(asos$spd)) * 100

# missingness of wind direction
drctn.missing = (sum(is.na(asos$drctn))/length(asos$drctn)) * 100

# missingness of precip
precip.missing = (sum(is.na(asos$precip_1hr_mm))/length(asos$precip_1hr_mm)) * 100

# gust mph
gust.missing = (sum(is.na(asos$gust_mph))/length(asos$gust_mph)) * 100

# wx codes
wxcodes.missing = (sum(is.na(asos$wxcodes))/length(asos$wxcodes)) * 100

missing_df = data.frame(vis.missing, tmpc.missing, dwptc.missing, dwptc.missing, 
           spd.missing, drctn.missing, gust.missing, precip.missing, wxcodes.missing)

## The results are that the asos data is pretty bad. Most of it is missing
# with the exception of visibility, wind speed, and direction.
# These are the only variables worth sourcing from asos.
##

asos_sub = asos[,c('station', 'valid_utc', 'lat', 'lon', 'vis', 'spd', 'drctn')]
# There is some strange values in the spd and and vis vars
asos_sub = asos_sub[which(asos_sub$vis <= 20),]
asos_sub = asos_sub[which(asos_sub$spd <= 75),]
summary(asos_sub)

# Let's try a lattice plot of histograms for the visibility data by station
# 
lattice::xyplot(vis~valid_utc | factor(station), data=asos_sub)

# what about if I restrict it down to April
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='04'))
# May?
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='05'))
# June?
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='06'))
# July?
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='07'))


# QQ plot between lafayette and Indy show a quadratic relationship. This makes some sense.
lattice::qq(station ~ vis, aspect = 1, data = asos_sub,
   subset = (asos_sub$station == "IND" | asos_sub$station == "LAF"))
