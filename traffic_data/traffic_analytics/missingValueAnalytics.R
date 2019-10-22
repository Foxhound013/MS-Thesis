library(lattice)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(RColorBrewer)
# library(zoo)
# library(xts)

# This script is intended to explore the missingness in the traffic speed data from INRIX

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

# To get a feel for the missigness of the data, we need to create a time series
# that is complete on this range and fill in the rows with NA by groups (xdid) 
# tidyr will help with its complete function

sf_traffic <- complete(sf_traffic, xdid, tstamp)
sf_traffic[which(is.na(sf_traffic$speed)),] # get a quick look at missing columns

# some of the metadata can very quickly and easily be filled
sf_traffic <- sf_traffic %>% group_by(xdid) %>% 
  fill(c(version, roadNumber, roadName, county, miles, bearing, district,
         subdistrict, prevXdid, nextXdid, geometry))
# in all actuality, the metadata, really should be elsewhere and this step won't be needed anymore

# take another quick look at the missing data
sf_traffic[which(is.na(sf_traffic$speed)),] # get a quick look at missing columns

# only speed is missing. Now we need to evaluate it's missingness.

# Spatial missingness, count the NA's for each xdid
na_tseries <- sf_traffic %>% group_by(xdid, tstamp) %>% transmute(count=sum(is.na(speed)))

pdf('./figures/missingnessByTime.pdf')
xyplot(count~tstamp | factor(xdid), data=na_tseries, pch=16, col='deepskyblue3',
       layout=c(1,6,length(unique(na_tseries$xdid))/6))
dev.off()

#missingness across time alone
missById <- sf_traffic %>% group_by(xdid) %>% transmute(count=sum(is.na(speed))) %>% unique

plot(missById$xdid, missById$count)

missFreq <- data.frame(table(missById$count))
colnames(missFreq) <- c('Missed', 'Frequency')

png("./figures/FrequencyOfNumberOfMissingValues.png", width = 7, height = 3, units = 'in', res = 300)
barchart(Frequency~Missed, data=missFreq, col='deepskyblue3', main='Frequency of Missing Values',
         xlab='Number of Missing Values', ylab='Frequency',
         scales=list(x=list(rot=90, cex=.6), y=list(cex=.6)))
dev.off()

# how do these missing values look on a map?
# should be able to merge the missing data by ID with the geometry from the original set
xdid.lon.lat <- traffic[,c('xdid','lon','lat')] %>% unique

miss.spatial <- missById %>% inner_join(xdid.lon.lat, by='xdid') %>% st_as_sf(coords=c('lon','lat'), crs=4326)

tmap_mode('view')
tm_shape(miss.spatial) + 
  tm_dots(col='count', breaks=c(0,10,25,50,100,200,500,1000,1400))

tm_shape(miss.spatial) + 
  tm_dots(title='Number of Missing Values', size='count', col='count', 
          palette=rev(brewer.pal(8, 'RdBu')), breaks=c(0,10,25,50,100,200,500,1000,1400)) +
  tm_scale_bar()

# Missing ratio, another way of encoding the amount missing
miss.spatial$ratio <- round(miss.spatial$count/length(unique(sf_traffic$tstamp)), 4)
missingQ <- quantile(miss.spatial$ratio, probs=seq(0,1,0.01))

png("./figures/missingRatio_QuantDistro.png", width = 4, height = 4, units = 'in', res = 300)
plot(missingQ, main='Quantile Distribution', ylab='Missingness Ratio (Missing/Possible)',
     xlab='Quantiles', pch=16, col='deepskyblue3', cex=.5, cex.main=.9, cex.axis=.7, cex.lab=.7)
dev.off()

# total missingness for each time slice to check for patterns there
totalMissing_tseries <- na_tseries %>% 
  group_by(tstamp) %>% summarise(totalNA = sum(count))

png('./figures/missingDatabyTime.png')
xyplot(totalNA~tstamp, data=totalMissing_tseries, pch=16, col='deepskyblue3', main='Missing Data by Time',
       ylab='Missing Value Count', xlab='Time Stamp', alpha=.5)
dev.off()

png('./figures/missingDatabyTime_smooth_ylim.png', width=500, height=500)
xyplot(totalNA~tstamp, data=totalMissing_tseries, pch=16, col='deepskyblue3', ylim=c(0,150),
       main='Missing Data by Time', ylab='Missing Value Count', xlab='Time Stamp',
       type=c("p","smooth"), col.line='red', lwd=2,
       alpha=.5)
dev.off()


summary(totalMissing_tseries$totalNA)
