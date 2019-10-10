library(lattice)
library(tidyverse)
library(RColorBrewer)
library(zoo)
library(xts)

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/traffic_data/2018-04-25_I65.csv'
traffic <- read.csv(fpath, header=F, stringsAsFactors=F, col.names=c('version', 'xdid', 'tstamp',
                    'speed', 'roadNumber', 'roadName', 'county', 'miles', 'lat', 'lon',
                    'bearing', 'district', 'subdistrict', 'prevXdid', 'nextXdid'))

traffic[1,1] <- sub(pattern='﻿',replacement='', x=traffic[1,1])

traffic$tstamp <- lubridate::as_datetime(traffic$tstamp)
traffic$hour <- lubridate::hour(traffic$tstamp)

traffic$xdid <- as.factor(traffic$xdid)
traffic$prevXdid <- as.factor(traffic$prevXdid)
traffic$nextXdid <- as.factor(traffic$nextXdid)

prev7 <- subset(traffic,subset = traffic$xdid=='1363485652')
prev6 <- subset(traffic,subset = traffic$xdid=='1363485639')
prev5 <- subset(traffic,subset = traffic$xdid=='1363485626')
prev4 <- subset(traffic,subset = traffic$xdid=='1363573605')
prev3 <- subset(traffic,subset = traffic$xdid=='1363430309')
prev2 <- subset(traffic,subset = traffic$xdid=='1363430296')
prev <- subset(traffic,subset = traffic$xdid=='1363430281')
center <- subset(traffic,subset = traffic$xdid=='1363430269')
nextSeg <- subset(traffic,subset = traffic$xdid=='1363430257')
nextSeg2 <- subset(traffic,subset = traffic$xdid=='1363485611')
nextSeg3 <- subset(traffic,subset = traffic$xdid=='1363583278')
nextSeg4 <- subset(traffic,subset = traffic$xdid=='1363583293')
nextSeg5 <- subset(traffic,subset = traffic$xdid=='1363583280')
nextSeg6 <- subset(traffic,subset = traffic$xdid=='1363583237')
nextSeg7 <- subset(traffic,subset = traffic$xdid=='1363583252')


seqSegs <- rbind(prev7, prev6, prev5, prev4, prev3, prev2, prev,
                 center, nextSeg, nextSeg2, nextSeg3, nextSeg4, nextSeg5,
                 nextSeg6, nextSeg7)
seqSegs$tstamp <- lubridate::as_datetime(seqSegs$tstamp)
segmentSequence <- c('1363485652', '1363485639', '1363485626', '1363573605', '1363430309',
                     '1363430296', '1363430281', '1363430269', '1363430257', '1363485611',
                     '1363583278', '1363583293', '1363583280', '1363583237', '1363583252')
seqSegs$xdid <- factor(seqSegs$xdid, levels=segmentSequence)

# This setup will be an excellent style as long as you can get the xdids ordered properly
lattice::xyplot(speed~tstamp | factor(xdid)*factor(hour), data=seqSegs, type='l',
                layout=c(1,7,24))

lattice::xyplot(speed~tstamp | factor(xdid), data=seqSegs, type='l',
                layout=c(1,7))



cRamp <- colorRampPalette( c('red', 'orange', 'yellow', 'grey', 'green'))

# this command seems to fail for some reason. It works just fine when operating on the 
# 15 minute aggregate data.
# pdf('speedByTslice.pdf')
# lattice::levelplot(speed~lon*lat | factor(tstamp), 
#                    data=seqSegs, layout=c(3,3),
#                    par.settings = list(regions = list(col = cRamp(100))))
# dev.off()


# the data is pretty noisy here, let's aggregate into 15 minute bins.

seqSegs15 <- seqSegs %>%
  group_by(xdid=xdid,
           agg_time=cut(tstamp, '15 min'), 
           prevXdid=prevXdid, 
           nextXdid=nextXdid,
           lon=lon, lat=lat) %>%
  summarize(agg_speed=mean(speed))

seqSegs15$xdid <- factor(seqSegs15$xdid, levels=segmentSequence)
seqSegs15$agg_time <- lubridate::as_datetime(seqSegs15$agg_time)




lattice::xyplot(agg_speed~agg_time | factor(xdid), data=seqSegs15, type='l',
                layout=c(1,15), par.strip.text=list(cex=.53),
                xlab='Time', ylab='Speed')

lattice::levelplot(agg_speed~lon*lat | factor(lubridate::hour(agg_time)), 
                   data=seqSegs15)

lattice::levelplot(agg_speed~lon*lat | factor(agg_time), data=seqSegs15, pch=16,
                   layout=c(6,1), type='l')

pdf('test.pdf')
lattice::levelplot(agg_speed~lon*lat | factor(agg_time), 
                   data=seqSegs15, layout=c(6,1), 
                   xlab='Longitude', ylab='Latitude',
                   par.settings = list(regions = list(col = cRamp(100))),
                   par.strip.text=list(cex=.53))
dev.off()

splommable <- seqSegs15[,c('xdid', 'agg_time', 'agg_speed')]
png('test2.png', height=1000, width=800)
lattice::splom(splommable, groups=splommable$xdid, pch=16, cex=.8)
dev.off()



# up to this point, I haven't checked for any potentially missing data and based on what I'm 
# seeing, there is definitely some missing data (check the obs available for each segment
# they're different from one another.)

# get a date time sequence put together
timeSeq = seq(from=lubridate::as_datetime('2018-05-01 00:00:00'),
    to=lubridate::as_datetime('2018-05-02 00:00:00'), by='1 min')

center.full <- merge(x=data.frame(tstamp=timeSeq),
                     y=center,
                     all.x = T)
plot(center.full$tstamp, center.full$speed, type='l')

center.full <- center.full %>% fill(c(xdid, version, roadNumber, roadName, county,
                                      bearing, district, subdistrict, prevXdid, nextXdid))

summary(center.full)

# to back fill the entire dataset with missing timesteps
# args are df in question, grouping column, column to complete
traffic.full <- complete(traffic, xdid, tstamp)

anyNA(traffic.full)
summary(traffic.full)

#show there is in fact missing data
center.f2 <- subset(traffic.full,subset = traffic.full$xdid=='1363430269')
plot(center.f2$tstamp, center.f2$speed, type='l')



summary(traffic.full)
(sum(is.na(traffic.full$speed))/length(traffic.full$speed))*100
# only 1.6% of the data is missing.

# Need to apply a method for imputing these missing values
# some of these will be as simple as grouping by xdid and applying the fill function

traffic.full <- traffic.full %>% group_by(xdid) %>% 
  fill(c(version, roadNumber, roadName, county, miles, lat, lon, bearing, district,
         subdistrict, prevXdid, nextXdid))

# for imputing the speed data, something like using a local average should suffice
# given that the data will be binned in 15 minute intervals.

# can try the imputeTS package or forecast to fill the NAs in the time series










# giving xts a try

traffic.xts <- as.xts(traffic, order.by=traffic$tstamp)
summary(traffic.xts)
anyNA(traffic.xts)
