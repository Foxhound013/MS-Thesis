library(lattice)
library(dplyr)
library(tidyr)
library(sf)

# This script has been developed to get a sample of the traffic data

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/traffic_data/2018-04-25_I65.csv'
traffic <- read.csv(fpath, header=F, stringsAsFactors=F, 
                    col.names=c('version', 'xdid', 'tstamp',
                                'speed', 'roadNumber', 'roadName', 'county', 'miles', 'lat', 'lon',
                                'bearing', 'district', 'subdistrict', 'prevXdid', 'nextXdid'))
traffic[1,1] <- sub(pattern='ï»¿',replacement='', x=traffic[1,1])
traffic$tstamp <- lubridate::as_datetime(traffic$tstamp)
# Need to characterize the missingness by space and time.

sf_traffic <- st_as_sf(traffic, coords=c('lon', 'lat'), crs=4326)
glimpse(sf_traffic) # check out the data


sf_traffic <- complete(sf_traffic, xdid, tstamp)
# some of the metadata can very quickly and easily be filled
sf_traffic <- sf_traffic %>% group_by(xdid) %>% 
  fill(c(version, roadNumber, roadName, county, miles, bearing, district,
         subdistrict, prevXdid, nextXdid, geometry))


byId <- sf_traffic %>% group_by(xdid)
traffic_sub <- sample_n(byId, 80, replace=T)

# order the data by tstamp
traffic_sub <- traffic_sub[order(traffic_sub$tstamp), ]

pdf('./figures/sampleSpeeds_80perID.pdf')
xyplot(speed~tstamp | factor(xdid), data=traffic_sub, type='l', layout=c(1,10), 
       par.strip.text=list(cex=.7), main='80 Random Samples by XDID', xlab='Time Stamp',
       ylab='Speed (MPH)')
dev.off()

