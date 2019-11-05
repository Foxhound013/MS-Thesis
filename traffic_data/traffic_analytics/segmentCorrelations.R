library(data.table)
library(sf)
library(tmap); tmap_mode('view')
library(mapview)
library(dplyr)
library(tidyr)
library(lattice)
library(zoo)
library(tseries)

##### Load the Data & Prep #####
fpath <- 'C:/Users/Downi/Desktop/June2018_I65_SpeedData.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
glimpse(traffic)

uniqueSegs <- traffic[,c('xdid','position','lon','lat')] %>% unique %>% st_as_sf(coords=c('lon','lat'))
#tm_shape(uniqueSegs) + tm_dots('position', style='cont')

# fix up the data
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='UTC')
traffic <- setDT(traffic)

# split data into north and south bound sets
traffic.n <- traffic[which(direction=='N'),]
traffic.n <- traffic.n[order(tstamp),]
traffic.n$position <- traffic.n$position %>% as.factor %>% droplevels

traffic.s <- traffic[which(direction=='S'),]
traffic.s <- traffic.s[order(tstamp),]
traffic.s$position <- traffic.s$position %>% as.factor %>% droplevels

# set the ordering for the position
summary(traffic.n$position)

uniqueSegs.n <- traffic.n[,c('xdid','position','lon','lat')] %>% unique %>% 
  st_as_sf(coords=c('lon','lat'))
uniqueSegs.n$position <- as.numeric(uniqueSegs.n$position)
tm_shape(uniqueSegs.n) + tm_dots('position', style='cont', breaks=c(0,100,200,300,400,500))

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

pdf('./figures/box&whisker_n_15Tseries.pdf')
bwplot(~speed | factor(position), data=traffic15.n, layout=c(1,6), col='deepskyblue3')
dev.off()


traffic15.n$day <- format(traffic15.n$tstamp, format="%d") %>% as.numeric

oneWeek <- traffic15.n[which(traffic15.n$day >= 3 & traffic15.n$day <=9),]
pdf('./figures/oneWeek_15N.pdf', width=16)
xyplot(speed~tstamp | factor(position), data=oneWeek, type='l', col='deepskyblue3',
       layout=c(1,6))
dev.off()
pdf('./figures/oneWeek_15N_Hist.pdf', width=16)
histogram(~speed | factor(position), data=oneWeek, col='deepskyblue3',
          layout=c(1,1))
dev.off()


##### Stationarity #####

adf.stats <- list()
count <- 1
for (i in unique(traffic15.n$position)){
  tmp <- adf.test(traffic15.n[which(position==i), speed], k=4)
  adf.stats[[count]] <- c('position'=i, 'statistic'=tmp$statistic, 'parameter'=tmp$parameter, 
                          'alternative'=tmp$alternative, 'p.value'=tmp$p.value, 'method'=tmp$method, 
                          'data.name'=tmp$data.name)

  count = count + 1
}
# clean up stats
adf.stats <- do.call(rbind, adf.stats) %>% as.data.frame
adf.stats$position <- as.numeric(as.character(adf.stats$position)) 
adf.stats$`statistic.Dickey-Fuller` <- as.numeric(as.character(adf.stats$`statistic.Dickey-Fuller`))
adf.stats$p.value <- as.numeric(as.character(adf.stats$p.value))


# png('./figures/adfTest_15N_daily.png')
# xyplot(position~`statistic.Dickey-Fuller`, data=adf.stats, pch=16, col='deepskyblue3',
#        main='Augmented Dickey-Fuller Test for all North Bound Segments',
#        xlab= 'Dickey-Fuller Value', ylab='Segment Position')
# dev.off()
#
png('./figures/adfTest_15N_hourly_pvals.png')
xyplot(position~p.value, data=adf.stats, pch=16, col='deepskyblue3', alpha=0.5,
       main='Hourly ADF Test for all North Bound Segments',
       xlab= 'P-Value', ylab='Segment Position')
dev.off()
#
# png('./figures/adfTest_15N_daily_pvals.png')
# xyplot(position~p.value, data=adf.stats, pch=16, col='deepskyblue3', alpha=0.5,
#        main='Daily ADF Test for all North Bound Segments',
#        xlab= 'P-Value', ylab='Segment Position')
# dev.off()
# 
# png('./figures/adfTest_15N_weekly_pvals.png')
# xyplot(position~p.value, data=adf.stats, pch=16, col='deepskyblue3', alpha=0.5,
#        main='Weekly ADF Test for all North Bound Segments',
#        xlab= 'P-Value', ylab='Segment Position')
# dev.off()

##### Time Series Analytics of One Segment #####
oneSeg.n <- traffic.n[which(position==250),] # Should be between Indy and Lafayette
twoSeg.n <- traffic.n[which(position==251),]
xyplot(speed~tstamp, oneSeg.n, type='l')

# correcting data to be stationary
adf.test(oneSeg.n$speed, k=96)
tmp <- diff(oneSeg.n$speed)
adf.test(tmp, k=96) # applying a lagged difference to 
plot(tmp, type='l')

adf.test(oneSeg.n$speed, k=672)
tmp <- diff(oneSeg.n$speed)
tmp <- diff(tmp) 
tmp <- diff(tmp) # requires a second order differencing to reach stationarity
adf.test(tmp, k=672) # applying a lagged difference to 
plot(tmp, type='l')


# test for stationarity
acf(oneSeg.n$speed)
pacf(oneSeg.n$speed)
ccf(oneSeg.n$speed, twoSeg.n$speed)

# look at 15 minute data
oneSeg.n <- traffic15.n[which(position==250),] # Should be between Indy and Lafayette
twoSeg.n <- traffic15.n[which(position==251),]
xyplot(speed~tstamp, oneSeg.n, type='l')

# convert to a zoo time series object
frequency(oneSeg.n$tstamp)
oneSeg.n <- zoo(oneSeg.n, order.by=oneSeg.n$tstamp)

# correlation testing


tmp <- acf(oneSeg.n$speed, main=paste0(250, 'oml;'))
pacf(oneSeg.n$speed)
ccf(oneSeg.n$speed, twoSeg.n$speed)


pdf('./figures/acf_all.pdf', width=16)
par(mfrow=c(2,3))
for (i in levels(traffic15.n$position)) {
  tmp <- acf(traffic15.n[which(position==i),speed], main=paste0('Segment ', i))
  
}
dev.off(); par(mfrow=c(1,1))


pdf('./figures/ccf_allVs250.pdf')
par(mfrow=c(2,2))
seg250 <- traffic15.n[which(position==250),speed]
for (i in unique(traffic15.n$position)){
  ccf(seg250, traffic15.n[which(position==i),speed])
}
dev.off()
par(mfrow=c(1,1))




Box.test(oneSeg.n$speed, type='Ljung-Box')
# H0: The data are independently distributed (i.e. the correlations in the population from which the 
# sample is taken are 0, so that any observed correlations in the data result from randomness of the 
# sampling process).
# Ha: The data are not independently distributed; they exhibit serial correlation.


