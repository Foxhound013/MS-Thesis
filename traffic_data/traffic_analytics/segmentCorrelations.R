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
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)

##### Load the Data & Prep #####
fpath <- 'C:/Users/Downi/Desktop/June_I-65_2018.csv'
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
traffic2.n.tsibble <- as_tsibble(traffic2.n, key='position', index='tstamp')
glimpse(traffic2.n.tsibble)
head(traffic2.n.tsibble)

traffic2.n.250 <- traffic2.n.tsibble %>% dplyr::filter(position == 250)
traffic2.n.250 %>% autoplot(speed)

traffic250.decomp <- traffic2.n.250 %>% STL(speed ~ season(period=c(day_freq, week_freq)))
traffic250.decomp %>% autoplot(speed)

traffic250.decomp[,c('position', 'tstamp', 'remainder')] %>% 
  ACF(remainder) %>% autoplot()
# the acf plot shows that the remainder is definitely not white noise


traffic2.n.250 %>% model(ARIMA(speed)) %>% forecast(h='10 days') %>% autoplot()



# the idea of these is sound but they take entirely too long. 
# fit.trend <- traffic2.n.250 %>%
#   model(trend_model = TSLM(speed ~ trend() ))
# start <- Sys.time()
# fit.day <- traffic2.n.250 %>%
#   model(trend_model = TSLM(speed ~ season(period=c(day_freq)) ))
# end <- Sys.time(); end-start
# start <- Sys.time()
# fit.week <- traffic2.n.250 %>%
#   model(trend_model = TSLM(speed ~ season(period=c(week_freq )) ))
# end <- Sys.time(); end-start

fit %>% forecast(h='30 days') %>% 
  autoplot()

fit.fcst <- fit %>% forecast(h='30 days')

# now try a simple linear regression model with trend and season
fit <- traffic2.n.250 %>% 
  model(trend_model=TSLM(speed~trend() + season(period=c(day_freq, week_freq))))
head(fit)
fit %>% forecast(h='10 days') %>% autoplot()

traffic2.n.250 %>% model(SNAIVE(speed~lag('week'))) %>% 
  forecast(h='10 days') %>% autoplot()





# attempt a Fourier approach.
data.spec <- spectrum(traffic2.n.250$speed) # same as TSA periodogram (diff graph)
spec.df <- data.frame(freq=data.spec$freq, spec=data.spec$spec)
spec.df <- spec.df[order(-spec.df$spec),]
spec.df$time = 1/(spec.df$freq)
head(spec.df)

data.spec.p <- spec.pgram(traffic2.n.250$speed)
spec.df2 <- data.frame(freq=data.spec.p$freq, spec = data.spec.p$spec)
spec.df2 <- spec.df2[order(-spec.df2$spec),]
spec.df2$time <- 1/(spec.df2$freq)
head(spec.df2)

p <- TSA::periodogram(traffic2.n.250$speed - mean(traffic2.n.250$speed))
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
order$time = 1/(order$freq)
head(order)
# above shows a beautiful justification for seasonality selection.

# quickly revisit stl with new frequencies
traffic250.decomp <- traffic2.n.250 %>% STL(speed ~ season(period=c(360, 720, 5400)))
traffic250.decomp %>% autoplot(speed)

traffic250.decomp %>% ACF(remainder) %>% autoplot()

traffic2.n.250$detrend <- traffic2.n.250$speed - traffic250.decomp$trend

traffic2.n.250 %>% autoplot(detrend)
adf.test(traffic2.n.250$detrend)
kpss.test(traffic2.n.250$detrend)
acf(traffic2.n.250$detrend)
pacf(traffic2.n.250$detrend)


traffic2.n.250.fft <- fft(traffic2.n.250$speed)
plot(seq(1,21600), traffic2.n.250.fft)


msts.250 <- msts(traffic2.n.250$speed, seasonal.periods=c(720, 5040))

fit <- tslm(msts.250 ~ trend + fourier(msts.250, K=c(3,3)))
fit <- 
msts.lm.fcst <- forecast(fit, data.frame(fourier(msts.250, K=c(3,3), h=5040)))
plot(forecast(fit, h=100, xreg=fourier(msts.250, K=c(3,3))))

fit <- TSLM(msts.250 ~ trend + season + fourier(msts.250, K=c(3,3)))



# attempt stl with finer control using stlplus
traffic2.n.250
weekDays <- c("Sunday", "Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday")
traffic2.n.250$weekDay <- weekdays(traffic2.n.250$tstamp) %>% as.factor()
levels(traffic2.n.250$weekDay) <- weekDays

tmp <- stlplus::stlplus(x=traffic2.n.250$speed,
                        t=traffic2.n.250$tstamp,
                        n.p=5040,
                        s.window=5041,
                        t.degree=0,
                        s.label=traffic2.n.250$weekDay)
plot(tmp)
trafficDaily <- traffic2.n.250
trafficDaily$speed <- trafficDaily$speed - tmp$data$seasonal - tmp$data$trend
xyplot(speed ~ tstamp, data=trafficDaily, col='deepskyblue3', type='l')

acf(traffic2.n.250$speed, lag.max=21600)
pacf(traffic2.n.250$speed, lag.max=21600)
acf(trafficDaily$speed, lag.max=21600)
pacf(trafficDaily$speed, lag.max=21600)


p2 <- TSA::periodogram(tmp$data$seasonal - mean(tmp$data$seasonal))
dd2 = data.frame(freq=p2$freq, spec=p2$spec)
order2 = dd[order(-dd2$spec),]
order2$time = 1/(order2$freq)
head(order2)

#png('./figures/dailySpeeds.png')
xyplot(speed ~ tstamp | factor(weekDay, levels = weekDays), data=traffic2.n.250, pch=16, 
       col='deepskyblue3', alpha=0.5, layout=c(7,1))
#dev.off()
