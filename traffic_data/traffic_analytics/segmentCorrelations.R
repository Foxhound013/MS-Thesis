library(data.table)
library(sf)
library(tmap); tmap_mode('view')
library(mapview)
library(dplyr)
library(tidyr)
library(lattice)
library(zoo)
library(xts)
library(tseries)
library(forecast)

##### Load the Data & Prep #####
fpath <- 'C:/Users/Downi/Desktop/June2018_I65_SpeedData.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
glimpse(traffic)

#uniqueSegs <- traffic[,c('xdid','position','lon','lat', 'direction')] %>% unique
#write.csv(uniqueSegs, './data/processed/uniqueSegs_I65.csv', row.names=F)
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

# check the ordering for the position
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
plot(oneSeg.n$speed, twoSeg.n$speed, pch=16)

#15 min
oneSeg.n.ts <- ts(oneSeg.n[,'speed'], frequency=1, start=as.integer(oneSeg.n$tstamp[1]))
oneSeg.components <- decompose(oneSeg.n.ts)
plot(oneSeg.components)

#hourly
oneSeg.n.ts <- ts(oneSeg.n[,'speed'], frequency=4, start=as.integer(oneSeg.n$tstamp[1]))
oneSeg.components <- decompose(oneSeg.n.ts)
plot(oneSeg.components)

#daily
oneSeg.n.ts <- ts(oneSeg.n[,'speed'], frequency=96, start=as.integer(oneSeg.n$tstamp[1]))
oneSeg.components <- decompose(oneSeg.n.ts)
plot(oneSeg.components)

# weekly
oneSeg.n.ts <- ts(oneSeg.n[,'speed'], frequency=672, start=as.integer(oneSeg.n$tstamp[1]))
oneSeg.components <- decompose(oneSeg.n.ts)
plot(oneSeg.components)

# removing the seasonal and residual components will leave you with a time series of the trend data.=
tmp <- oneSeg.n.ts - oneSeg.components$seasonal - oneSeg.components$random
plot(tmp)


# correcting data to be stationary
adf.test(oneSeg.n$speed, k=96)
tmp <- diff(oneSeg.n$speed)
adf.test(tmp, k=96)
plot(tmp, type='l')

adf.test(oneSeg.n$speed, k=672)
tmp <- diff(oneSeg.n$speed)
adf.test(tmp, k=672)
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






##### 2 minute time series #####


# if you take all of the meta info that repeats and put it in the by argument, it'll only occur once.
traffic2.n <- traffic.n[,list(speed=mean(speed)), 
                        by=list(xdid=xdid, 
                                position=position, 
                                tstamp=as.POSIXct(cut(tstamp, '2 min'),tz='UTC'),
                                lat=lat,
                                lon=lon,
                                roadname=roadname,
                                direction=direction,
                                bearing=bearing)]
hour_freq <- 60/2
day_freq <- hour_freq*24
week_freq <- day_freq*7

# grab speeds from one segment
traffic2.n.250 <- traffic2.n[which(position==250),]
plot(traffic2.n.250$tstamp, traffic2.n.250$speed, type='l')

traffic2.n.msts <- msts(traffic2.n.250$speed,
                        seasonal.periods=c(day_freq, week_freq))

plot(traffic2.n.msts)
decompose(traffic2.n.msts) %>% autoplot()
mstl(traffic2.n.msts) %>% autoplot()

traffic2.n.decomposed <- mstl(traffic2.n.msts)
fit <- tbats(y=traffic2.n.msts)
plot(forecast(fit))



traffic2.n.tsibble <- tsibble::as_tsibble(traffic2.n, key='position', index='tstamp')




















tstamps <- traffic2.n.250$tstamp
traffic2.n.xts <- xts(traffic2.n.250$speed, 
                      order.by=tstamps, 
                      tzone='UTC',
                      frequency=week_freq)
colnames(traffic2.n.xts) <- c('speed')
traffic2.n.ts <- ts(traffic2.n.xts)
plot.xts(traffic2.n.ts)
ts(traffic2.n.xts)
decompose(traffic2.n.xts) %>% autoplot()

traffic2.n.ts <- ts(traffic2.n.250$speed, frequency=week_freq,
                    start=c(2018,1))
plot(traffic2.n.ts)
traffic2.n.week <- ts(traffic2.n.250$speed, frequency=week_freq,
                      start=as.integer(traffic2.n.250$tstamp[1]),
                      end=as.integer(traffic2.n.250$tstamp[10]))
traffic2.n.week.decomp <- decompose(traffic2.n.week)
traffic2.n.week.decomp %>% autoplot()

traffic2.n.week.decomp$x - traffic2.n.week.decomp$seasonal

traffic2.n.250 <- msts(traffic2.n.250$speed, 
                       seasonal.periods=c(day_freq, week_freq),
                       start=as.integer(traffic2.n.250$tstamp[1]),
                       end=as.integer(traffic2.n.250$tstamp[100]))
plot(traffic2.n.250)
decompose(traffic2.n.250) %>% autoplot()
#NOTE: YOUR TIME OBJECTS ARE WRONG WHEN GOING TO TS OBJECTS
# NEED TO SPECIFY THE END AND TIMESTEP IF YOU CAN

# the hourly frequency doesn't change anything about the trend.
# weekly or daily on their own still leave the trend exhibiting
# seasonality.
traffic2.n.stl <- mstl(traffic2.n.250)
plot(traffic2.n.stl)
traffic2.n.stl %>% autoplot()

traffic2.n.stl.df <- as.data.frame(traffic2.n.stl)

lm.stl <- lm(Data~Seasonal720+Seasonal5040, data=traffic2.n.stl.df)

tslm(Data~., data=traffic2.n.stl) %>% autoplot()

tmp %>% forecast(method='arima') %>% autoplot() 

tmp2 <- tbats(traffic2.n.stl[,1])
plot(forecast(tmp))


tmp2 <- tbats(traffic2.n.250)
plot(tmp2)

tmp3 <- tmp2 %>% forecast

