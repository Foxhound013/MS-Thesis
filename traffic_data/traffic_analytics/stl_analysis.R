library(data.table)
library(lattice)
library(dplyr)
library(tidyr)
library(stlplus)


##### Load the Data & Prep #####
fpath <- 'C:/Users/Downi/Desktop/June_I-65_2018.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
#glimpse(traffic)

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

# initialize a ts object for one of the segments to test on.

seg250 <- traffic2.n %>% filter(position==250)


seg250.ts <- ts(seg250$speed, start=as.integer(seg250$tstamp[1]), frequency=720)
seg250.xts <- xts::xts(x=seg250$speed, order.by=seg250$tstamp) # xts implementation
plot(seg250.xts) # the time stamps are very wrong in the ts object

pdf('./figures/seg250_speedTSeries.pdf')
xyplot(seg250.xts, type='l',
       superpose=F,
       cut = list(number = 30, overlap = 0),
       layout=c(1,7))
dev.off()

seg250.stl <- stlplus(seg250$speed, 
                      t=seg250$tstamp, 
                      n.p=30,
                      s.window=29,
                      s.degree=2,
                      outer=5,
                      details=T)

plot(seg250.stl)

seas.xts <- xts::as.xts(x=seg250.stl$data$seasonal, order.by=seg250.stl$time)
seas.xts[,2] <- weekdays(seg250.stl$time)

pdf('./figures/seg250_seasonalTSeries.pdf')
xyplot.ts(seas.xts, type='l',
          superpose=T,
          cut = list(number = 60, overlap = 0),
          layout=c(1,8,1))
dev.off()

xyplot(speed~tstamp, data=seg250, ylim=c(55, 95), type='l', layout=c(1,3,1))
pdf('./figures/STL_seasonalDiagnostic.pdf')
plot_seasonal(seg250.stl, layout=c(3,3))
dev.off()

plot_trend(seg250.stl)

pdf('./figures/STL_cycleDiagnositc.pdf')
plot_cycle(seg250.stl, layout=c(3,3))
dev.off()

seg250$day <- format(as.Date(seg250$tstamp,format="%Y-%m-%d"), format = "%d")
seg250$week <- week(seg250$tstamp)
xyplot(seasonal(seg250.stl)~seg250$week)

# shift focus to weekly seasonality

# diff between 7 day and 7.5 day seasonality is interesting.
seg250.sub <- seg250.stl$data$raw - seg250.stl$data$seasonal
seg250.sub.stl <- stlplus(seg250.sub,
                          t=seg250$tstamp,
                          n.p=5400,
                          s.window=5439,
                          outer=5)
plot(seg250.sub.stl)

plot_seasonal(seg250.sub.stl, layout=c(3,3,1))

plot_trend(seg250.sub.stl)

plot_cycle(seg250.sub.stl, layout=c(3,3,1))
