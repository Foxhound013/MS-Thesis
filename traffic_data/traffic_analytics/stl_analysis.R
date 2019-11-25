library(data.table)
library(lattice)
library(dplyr)
library(tidyr)
library(stlplus)


##### Load the Data & Prep #####
fpath <- 'C:/Users/Downi/Desktop/June_I-65_2018.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
glimpse(traffic)

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
xyplot(speed~tstamp, data=seg250, type='l')

seg250.ts <- ts(seg250$speed, start=as.integer(seg250$tstamp[1]), frequency=720)
seg250.xts <- xts::xts(x=seg250$speed, order.by=seg250$tstamp) # xts implementation
plot(seg250.xts) # the time stamps are very wrong in the ts object


seg250.stl <- stlplus(seg250$speed, 
                      t=seg250$tstamp, 
                      n.p=720,
                      s.window=719,
                      s.degree=1,
                      outer=5,
                      details=T)

plot(seg250.stl)
pdf('./figures/STL_seasonalDiagnostic.pdf')
plot_seasonal(seg250.stl, layout=c(3,3))
dev.off()

plot_trend(seg250.stl)

pdf('./figures/STL_cycleDiagnositc.pdf')
plot_cycle(seg250.stl, layout=c(3,3))
dev.off()

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
