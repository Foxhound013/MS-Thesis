library(data.table)
library(sf)
library(tmap); tmap_mode('view')
library(mapview)
library(dplyr)
library(tidyr)
library(lattice)

##### Load the Data & Prep #####
fpath <- 'C:/Users/Downi/Desktop/June2018_I65_SpeedData.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
glimpse(traffic)

uniqueSegs <- traffic[,c('xdid','position','lon','lat')] %>% unique %>% st_as_sf(coords=c('lon','lat'))
#tmap_mode('view')
#tm_shape(uniqueSegs) + tm_dots()

# fix up the data
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='UTC')
traffic <- setDT(traffic)

# split data into north and south bound sets
traffic.n <- traffic[which(direction=='N'),]
traffic.n <- traffic.n[order(tstamp),]
traffic.n$position <- as.factor(traffic.n$position)

traffic.s <- traffic[which(direction=='S'),]
traffic.s <- traffic.s[order(tstamp),]
traffic.s$position <- as.factor(traffic.s$position)

# set the ordering for the position
summary(traffic.n$position)
uniqueSegs.n <- traffic.n[,c('xdid','position','lat','lon')] %>% unique %>% 
  st_as_sf(coords=c('lon','lat'))

tm_shape(uniqueSegs.n) + tm_dots('position', style='cont')

##### Time Series Analytics #####

# plot the data as a time series and order panels by position number
# resulting pdf was ran through an online converter to reverse the order
start <- Sys.time()
pdf('./figures/orderedSegments_N_Tseries.pdf', width=16)
xyplot(speed~tstamp | factor(position), data=traffic.n, type='l', col='deepskyblue3',
       layout=c(1,6), ylim=c(0,90), scales=list(y=list(at=seq(0,90,15))))
dev.off(); end <- Sys.time(); end-start

# Since we're planning on doing a 15 minute aggregation to smooth out the data some
# and get away from some of the noise, average the data into 15 minute bins.

# if you take all of the meta info that repeats and put it in the by argument, it'll only occur once.
traffic15.n <- traffic.n[,list(speed=mean(speed)), 
                         by=list(xdid=xdid, 
                                 position=position, 
                                 tstamp=as.POSIXct(cut(tstamp, '15 min'),tz='UTC'),
                                 lat=lat,
                                 lon=lon,
                                 roadname=roadname,
                                 direction=direction,
                                 bearing=bearing)]
# verify validity of subset
length(unique(traffic15.n$xdid))
length(unique(traffic.n$xdid))

start <- Sys.time()
pdf('./figures/orderedSegments_N_15Tseries.pdf', width=16)
xyplot(speed~tstamp | factor(position), data=traffic15.n, type='l', col='deepskyblue3',
       layout=c(1,6), ylim=c(0,90), scales=list(y=list(at=seq(0,90,15))))
dev.off(); end <- Sys.time(); end-start

##### Correlation of One Segment #####
oneSeg.n <- traffic.n[which(position==250),] # Should be between Indy and Lafayette

# carry out a correlation analysis over time with this one segment.

