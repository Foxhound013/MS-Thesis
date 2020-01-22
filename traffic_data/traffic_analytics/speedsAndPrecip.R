library(data.table)
library(dplyr)
library(lattice)
library(hexbin)
library(sf)
library(tmap)

traffic <- fread('../../../../../Desktop/June_I-65_2018.csv',
                 col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                             'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
traffic <- traffic %>% filter(direction=='N')
# traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')

precip <- fread('../../../../../Desktop/6_out743.csv')
precip <- precip %>% filter(direction=='N')
precip$tstamp <- as.POSIXct(precip$tstamp, tz='utc', origin='1970-01-01 00:00:00')

# The traffic data is set to 1 minute resolution, scale down to 2 minute resolution
# in order to match the precip data
# traffic <- traffic %>% filter((minute(tstamp)%%2)==0) %>% 
#   left_join(precip, by=c('tstamp', 'lon', 'lat', 'position', 'xdid', 'direction'))

# estimate missing data from previous 1 minute data, then cut down to 2 minute data only
traffic <- traffic %>% mutate(tstamp=as.POSIXct(tstamp, tz='utc', origin='1970-01-01 00:00:00')) %>% 
  group_by(position) %>% filter((minute(tstamp)%%2)==0) %>% 
  left_join(precip, by=c('tstamp', 'lon', 'lat', 'position', 'xdid', 'direction')) %>% 
  do(zoo::na.locf(.))

anyNA(traffic$precip)

road.map <- traffic %>% select(position,lon,lat) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()

# previuos method that would average 2 min slices
# setDT(traffic)
# traffic <- traffic[,list(speed=mean(speed)), 
#                    by=list(xdid=xdid, 
#                            position=position, 
#                            tstamp=as.POSIXct(cut(tstamp, '2 min'),tz='UTC'),
#                            lat=lat,
#                            lon=lon,
#                            roadname=roadname,
#                            direction=direction,
#                            bearing=bearing)]


# # locate missing values and impute
# tmp <- traffic[is.na(traffic$precip),] # two time slots are missing
# # impute missing values with most recent prior value
# traffic <- traffic %>% group_by(position) %>% do(zoo::na.locf(.))
# traffic <- traffic[order(traffic$tstamp),]
# tmp <- traffic[is.na(traffic$precip),]


pdf('./figures/precipVspeed.pdf')
xyplot(precip ~ speed | factor(position), data=traffic, pch=16, alpha=0.2,
       layout=c(3,3))
dev.off()

# traffic.numeric <- traffic[,c('speed', 'precip')]
# tmp <- prcomp(traffic.numeric, scale.=T)
# png('./figures/biplot.png')
# biplot(tmp)
# dev.off()
# 
# screeplot(tmp)

# clustering to discriminate between standard speeds and reduced speeds.

# tmp <- kmeans(traffic$speed, 2)
# tmp$tot.withinss
# traffic$cluster <- tmp$cluster
# pdf('./figures/precipVspeed_clust.pdf')
# xyplot(speed ~ precip | factor(position), data=traffic, groups=cluster, 
#        pch=16, layout=c(3,3))
# dev.off()


# rather than use k means to determine thresholds, use 55 as a threshold
# this will be roughly 10% less than 65, a typical speed limit for most of I-65
# 55 is the lowest postest speed limit on I-65

traffic.slow <- traffic %>% filter(speed <= 55)
traffic.fast <- traffic %>% filter(speed > 55)

nrow(traffic.fast)/nrow(traffic) * 100
nrow(traffic.slow)/nrow(traffic) * 100

# slow speeds make up a very small percentage of the dataset

# tmp <- traffic.slow %>% filter(position==250)
# xyplot(speed~precip | factor(position), data=traffic.slow,
#        layout=c(1,1,1))

# characterize the dataset via time slots
traffic$hours <- hour(traffic$tstamp)
traffic$mins <- minute(traffic$tstamp)

# daylight 1000 to 0115 UTC
traffic$daylight <- NA
traffic[which(((traffic$hours>=1 & traffic$mins>=15) | (traffic$hours > 1 & traffic$mins >=0)) & 
                (traffic$hours < 10) ),'daylight'] <- F
traffic[is.na(traffic$daylight),'daylight'] <- T

# tmp <- traffic %>% filter(position==250) # visual verification

traffic$dayofweek <- weekdays(traffic$tstamp) %>% 
  factor(levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

traffic$construction <- ifelse(traffic$startmm >= 8 & traffic$startmm <= 16 |
                                 traffic$startmm >= 50 & traffic$startmm <= 68 |
                                 traffic$startmm >= 141 & traffic$startmm <= 165 |
                                 traffic$startmm >= 167 & traffic$startmm <= 176 |
                                 traffic$startmm >= 197 & traffic$startmm <= 207 |
                                 traffic$startmm >= 229 & traffic$startmm <= 253,
                               yes=T, no=F)


png('./figures/precipVspeed_hourOfDay.png')
xyplot(precip~speed | factor(hours), data=traffic)
dev.off()

pdf('./figures/precipVspeed_hour_density.pdf', width=30)
densityplot(~speed | factor(hours), data=traffic, plot.points=F, groups=,
            layout=c(24,1))
dev.off()


# pdf('./figures/precipVspeed_dayofweek.pdf')
# xyplot(precip~speed | factor(dayofweek), data=traffic, pch=16, col='deepskyblue3', alpha=0.5,
#        layout=c(7,1), par.strip.text=list(cex=.8))
# dev.off()


# only defining the event this way here for density plots, redefined later.
traffic <- traffic %>% mutate(event=ifelse((precip>0), yes=T, no=F))

pdf('./figures/precipVspeed.pdf')
xyplot(precip ~ speed | factor(position), data=traffic, pch=16, alpha=0.2,
       group=event, auto.key=T, layout=c(3,3))
dev.off()

pdf('./figures/positionVspeed.pdf', width=8, height=10)
xyplot(position ~ speed, data=traffic, pch=16, alpha=0.2,
       group=event, auto.key=T)
dev.off()

spd.cut <- cut(traffic$speed,breaks=c(0,25,50,75,100),overlap=0)

pdf('./figures/precipVspeed_hour_density.pdf', width=15)
densityplot(~speed | factor(hours)*spd.cut, data=traffic, plot.points=F, 
            groups=event, auto.key=T, layout=c(12,2),
            xlim=c(0,90))
dev.off()



spd.cut <- cut(traffic$speed,breaks=seq(0,95,5))
pdf('./figures/precipVspeed_density.pdf', width=15)
densityplot(~speed | spd.cut, data=traffic, plot.points=F,
            groups=event, auto.key=T, layout=c(5,4))
dev.off()

pdf('./figures/bwplot_speedByHour.pdf', width=12)
bwplot(event~speed | factor(hours), data=traffic, auto.key=T,
       do.out=F, layout=c(12,2))
dev.off()


tmp.spd <- traffic %>% filter(speed <= 60)
pdf('./figures/precipVspeed_lowspeed_hour_density.pdf', width=30)
densityplot(~speed | factor(rev(hours)), data=traffic, plot.points=F, groups=event,
            layout=c(24,1,1))
dev.off()



# produce levels indicating precip levels for bw plots
tmp <- traffic
#tmp$precip <- as.integer(tmp$precip)
# tmp$rcat <- cut(tmp$precip, 10)
tmp$rcat <- cut(tmp$precip, breaks=c(-1,0,0.01,10,20,30,40,50,60,70,80,90,100,140), right=F, include.lowest=T)
tmp$section <- cut(tmp$position, breaks=c(0,100,200,300,400,500), right=F, include.lowest=T)

pdf('./figures/bwplot_speedByCat.pdf')
bp <- bwplot(rcat~speed | factor(section)*factor(construction), data=tmp, 
       ylab='Precip. Range (mm/hr)', xlab='Speed (mph)', 
       do.out=F, layout=c(1,5,2), cex=.3, scales=list(cex=.5),
       par.strip.text=list(cex=.7))
dev.off()


# levelplot(speed~position*hours | factor(dayofweek), data=tmp)


# calculate the quantities available to boxplot
for (i in seq(0,100,10)) {
  tmp.test2 <- tmp %>% filter(position >= 260 & position <= 360 & construction==T & precip == 0)
  tmp.test <- tmp %>% filter(position >= 260 & position <= 360 & construction==T & 
                               precip >= i & precip < i+10)
  
  baseline.md <- median(tmp.test2$speed)
  rainy.md <- median(tmp.test$speed)
  
  print(100 - (rainy.md / baseline.md)*100)
  # need to record sample sizes somehow
}




tmp.test2 <- tmp %>% filter(position >= 0 & position <= 100 & construction==F & precip == 0)
baseline <- quantile(tmp.test2$speed, probs=seq(0,1,.25))
baseline.md <- median(tmp.test2$speed)
baseline.mn <- mean(tmp.test2$speed)

tmp.test <- tmp %>% filter(position >= 0 & position <= 100 & construction==F & 
                             precip >= 30 & precip < 40)
rainy <- quantile(tmp.test$speed, probs=seq(0,1,.25))
rainy.md <- median(tmp.test$speed)
rainy.mn <- mean(tmp.test$speed)
sd(tmp.test$speed)
var(traffic$speed)

100 - (rainy.mn / baseline.mn)*100

# it may be worthwhile to write this as a loop and output to a df so that
# these values can be plotted against each other.
# how can -INF be interpreted?



# redefining an event.
traffic <- traffic %>% mutate(event=ifelse((precip>0 & speed <=55), yes=T, no=F))

# non-event defined as > 55 mph and no precip
nonevents.summary <- traffic %>% group_by(position) %>%
  summarize(sumNight=sum(ifelse((daylight==F & event==F), yes=1, no=0), na.rm=T),
            meanSpd_Night=mean(ifelse((daylight==F & event==F), yes=speed, no=NA), na.rm=T),
            varSpd_Night=var(ifelse((daylight==F & event==F), yes=speed, no=NA), na.rm=T),
            sdSpd_Night=sd(ifelse((daylight==F & event==F), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Night=mean(ifelse((daylight==F & event==F), yes=precip, no=NA), na.rm=T),
            varPrecip_Night=var(ifelse((daylight==F & event==F), yes=precip, no=NA), na.rm=T),
            sdPrecip_Night=sd(ifelse((daylight==F & event==F), yes=precip, no=NA), na.rm=T),
            
            sumDay=sum(ifelse((daylight==T & event==F), yes=1, no=0), na.rm=T),
            meanSpd_Day=mean(ifelse((daylight==T & event==F), yes=speed, no=NA), na.rm=T),
            varSpd_Day=var(ifelse((daylight==T & event==F), yes=speed, no=NA), na.rm=T),
            sdSpd_Day=sd(ifelse((daylight==T & event==F), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Day=mean(ifelse((daylight==T & event==F), yes=precip, no=NA), na.rm=T),
            varPrecip_Day=var(ifelse((daylight==T & event==F), yes=precip, no=NA), na.rm=T),
            sdPrecip_Day=sd(ifelse((daylight==T & event==F), yes=precip, no=NA), na.rm=T))

sum(nonevents.summary$sumNight)/(21600*458) * 100
sum(nonevents.summary$sumDay)/(21600*458) * 100

# Events defined as less than 55mph and precip
events.summary <- traffic %>% group_by(position) %>% 
  summarize(sumNight=sum(ifelse((daylight==F & event==T), yes=1, no=0), na.rm=T),
            meanSpd_Night=mean(ifelse((daylight==F & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_Night=var(ifelse((daylight==F & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_Night=sd(ifelse((daylight==F & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Night=mean(ifelse((daylight==F & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_Night=var(ifelse((daylight==F & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_EventsNight=sd(ifelse((daylight==F & event==T), yes=precip, no=NA), na.rm=T),
            
            sumDay=sum(ifelse((daylight==T & event==T), yes=1, no=0), na.rm=T),
            meanSpd_Day=mean(ifelse((daylight==T & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_Day=var(ifelse((daylight==T & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_Day=sd(ifelse((daylight==T & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Day=mean(ifelse((daylight==T & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_Day=var(ifelse((daylight==T & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_Day=sd(ifelse((daylight==T & event==T), yes=precip, no=NA), na.rm=T),)

sum(events.summary$sumNight)/(21600*458) * 100 
sum(events.summary$sumDay)/(21600*458) * 100

# what is the difference? Negative indicates variable in question was higher during an event.
normVevent <- round((nonevents.summary - events.summary), 2)
normVevent$position <- events.summary$position


plot(x=events.summary$meanSpd_Day, y=events.summary$position)
points(x=nonevents.summary$meanSpd_Day,y=nonevents.summary$position, col='red')

tmp <- events.summary %>% select(position, meanSpd_Day) %>% left_join(nonevents.summary, by='position') %>% 
  select(position, meanSpd_Day.x, meanSpd_Day.y)
bp <- boxplot(tmp$meanSpd_Day.x, tmp$meanSpd_Day.y, notch=T, outline=T, col=c('red','blue'))

bwplot(~meanSpd_Day, data=events.summary, xlim=c(0,80))
bwplot(~meanSpd_Day, data=nonevents.summary, xlim=c(0,80))

plot(normVevent, pch=16, col='blue', cex=.1)

# splom(normVevent, pch=16, alpha=0.5,
#       scales=list(cex=0.01)
#       )

xyplot(meanSpd_Day~meanSpd_Night, data=normVevent)

tmp.events <- events.summary %>% select(position, meanSpd_Night, meanSpd_Day)
tmp.nonEvents <- nonevents.summary %>% select(position, meanSpd_Night, meanSpd_Day)

eventVnon_percent <- 100 - (tmp.events/tmp.nonEvents) * 100
eventVnon_percent$position <- tmp.events$position

eventPrecip <- events.summary %>% select(position, meanPrecip_Night, meanPrecip_Day)
eventVnon_percent <- eventVnon_percent %>% left_join(eventPrecip, by='position')

xyplot(meanPrecip_Night~meanSpd_Night, data=eventVnon_percent, pch=16,
       main='Percent Speed Reduction from Normal at Night',
       xlab='Percent Speed Reduction From Normal', ylab='Mean Precipitation (mm/hr)',
       xlim=c(0,70), ylim=c(0,70),
       scales=list(x=list(at=seq(0,70,5)), y=list(at=seq(0,70,10))))

densityplot(~meanSpd_Night, data=eventVnon_percent, pch=16,
            main='Density Estimation of Speed Reduction from Normal at Night',
            xlab='Percent Speed Reduction From Normal',
            xlim=c(0,80), ylim=c(0,.1),
            scales=list(x=list(at=seq(0,80,5)), y=list(at=seq(0,.1,0.01))))


xyplot(meanPrecip_Day~meanSpd_Day, data=eventVnon_percent, pch=16,
       main='Percent Speed Reduction from Normal during Day',
       xlab='Percent Speed Reduction From Normal', ylab='Mean Precipitation (mm/hr)',
       xlim=c(0,70), ylim=c(0,70),
       scales=list(x=list(at=seq(0,70,5)), y=list(at=seq(0,70,10))))

densityplot(~meanSpd_Day, data=eventVnon_percent, pch=16,
            main='Density Estimation of Speed Reduction from Normal during Day',
            xlab='Percent Speed Reduction From Normal',
            xlim=c(0,80), ylim=c(0,.1),
            scales=list(x=list(at=seq(0,80,5)), y=list(at=seq(0,.1,0.01))))

smoothScatter(eventVnon_percent$meanSpd_Day, eventVnon_percent$meanPrecip_Day)


plot(events.summary$position, events.summary$sumNight, col='blue', pch=16)
points(events.summary$position, events.summary$sumDay, col='red', pch=16)

plot(events.summary$position, events.summary$meanSpd_Night, col='blue', pch=16)
points(events.summary$position, events.summary$meanSpd_Day, col='red', pch=16)

plot(events.summary$position, events.summary$meanPrecip_Night, col='blue', pch=16)
points(events.summary$position, events.summary$meanPrecip_Day, col='red', pch=16)

plot(events.summary$position, events.summary$sdPrecip_Night, col='blue', pch=16)
points(events.summary$position, events.summary$sdPrecip_Day, col='red', pch=16)



# form a count of each speed classification 0 - 90
speed.count <- data.frame(speed=seq(0,95,1))









# set construction zones as T/F column
traffic.nc <- traffic
traffic.nc$construction <- ifelse(traffic.nc$startmm >= 8 & traffic.nc$startmm <= 16 |
                                       traffic.nc$startmm >= 50 & traffic.nc$startmm <= 68 |
                                       traffic.nc$startmm >= 141 & traffic.nc$startmm <= 165 |
                                       traffic.nc$startmm >= 167 & traffic.nc$startmm <= 176 |
                                       traffic.nc$startmm >= 197 & traffic.nc$startmm <= 207 |
                                       traffic.nc$startmm >= 229 & traffic.nc$startmm <= 253,
                                     yes=T, no=F)
# remove construction
traffic.nc <- traffic.nc %>% filter(construction==F)


pdf('./figures/positionVspeed_nc.pdf', width=8, height=10)
xyplot(position ~ speed, data=traffic.nc, pch=16, alpha=0.2,
       group=event, auto.key=T)
dev.off()

pdf('./figures/positionVspeed_nc_hours.pdf', width=10, height=8)
xyplot(position ~ speed | factor(hours)*factor(construction), data=traffic, 
       pch=16, alpha=0.2, group=event, auto.key=T, layout=c(12,1,4))
dev.off()

pdf('./figures/positionVspeed_nc_hours&day.pdf', width=10, height=8)
xyplot(position ~ speed | factor(dayofweek)*factor(hours), data=traffic.nc, pch=16, alpha=0.2,
       group=event, auto.key=T, layout=c(7,2))
dev.off()

# produce levels indicating precip levels for bw plots
tmp <- traffic.nc
#tmp$precip <- as.integer(tmp$precip)
# tmp$rcat <- cut(tmp$precip, 10)
tmp$rcat <- cut(tmp$precip, breaks=c(-1,0,10,20,30,40,50,60,70,80,90,100,140))
tmp$section <- cut(tmp$position, breaks=c(0,100,200,300,400,500))

pdf('./figures/bwplot_speedByCat_nc.pdf')
bwplot(rcat~speed | factor(section), data=tmp, ylab='Precip. Range (mm/hr)', xlab='Speed (mph)', 
       do.out=F, layout=c(1,5), cex=.3, scales=list(cex=.5))
dev.off()


nonevents.summary <- traffic.nc %>% group_by(position) %>%
  summarize(sumNight=sum(ifelse((daylight==F & event==F), yes=1, no=0), na.rm=T),
            meanSpd_Night=mean(ifelse((daylight==F & event==F), yes=speed, no=NA), na.rm=T),
            varSpd_Night=var(ifelse((daylight==F & event==F), yes=speed, no=NA), na.rm=T),
            sdSpd_Night=sd(ifelse((daylight==F & event==F), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Night=mean(ifelse((daylight==F & event==F), yes=precip, no=NA), na.rm=T),
            varPrecip_Night=var(ifelse((daylight==F & event==F), yes=precip, no=NA), na.rm=T),
            sdPrecip_Night=sd(ifelse((daylight==F & event==F), yes=precip, no=NA), na.rm=T),
            
            sumDay=sum(ifelse((daylight==T & event==F), yes=1, no=0), na.rm=T),
            meanSpd_Day=mean(ifelse((daylight==T & event==F), yes=speed, no=NA), na.rm=T),
            varSpd_Day=var(ifelse((daylight==T & event==F), yes=speed, no=NA), na.rm=T),
            sdSpd_Day=sd(ifelse((daylight==T & event==F), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Day=mean(ifelse((daylight==T & event==F), yes=precip, no=NA), na.rm=T),
            varPrecip_Day=var(ifelse((daylight==T & event==F), yes=precip, no=NA), na.rm=T),
            sdPrecip_Day=sd(ifelse((daylight==T & event==F), yes=precip, no=NA), na.rm=T))

sum(nonevents.summary$sumNight)/(21600*458) * 100
sum(nonevents.summary$sumDay)/(21600*458) * 100

# Events defined as less than 55mph and precip
events.summary <- traffic.nc %>% group_by(position) %>% 
  summarize(sumNight=sum(ifelse((daylight==F & event==T), yes=1, no=0), na.rm=T),
            meanSpd_Night=mean(ifelse((daylight==F & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_Night=var(ifelse((daylight==F & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_Night=sd(ifelse((daylight==F & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Night=mean(ifelse((daylight==F & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_Night=var(ifelse((daylight==F & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_EventsNight=sd(ifelse((daylight==F & event==T), yes=precip, no=NA), na.rm=T),
            
            sumDay=sum(ifelse((daylight==T & event==T), yes=1, no=0), na.rm=T),
            meanSpd_Day=mean(ifelse((daylight==T & event==T), yes=speed, no=NA), na.rm=T),
            varSpd_Day=var(ifelse((daylight==T & event==T), yes=speed, no=NA), na.rm=T),
            sdSpd_Day=sd(ifelse((daylight==T & event==T), yes=speed, no=NA), na.rm=T),
            
            meanPrecip_Day=mean(ifelse((daylight==T & event==T), yes=precip, no=NA), na.rm=T),
            varPrecip_Day=var(ifelse((daylight==T & event==T), yes=precip, no=NA), na.rm=T),
            sdPrecip_Day=sd(ifelse((daylight==T & event==T), yes=precip, no=NA), na.rm=T),)

sum(events.summary$sumNight)/(21600*458) * 100 
sum(events.summary$sumDay)/(21600*458) * 100

# what is the difference? Negative indicates variable in question was higher during an event.
normVevent <- round((nonevents.summary - events.summary), 2)
normVevent$position <- events.summary$position

plot(normVevent, pch=16, col='blue')

# splom(normVevent, pch=16, alpha=0.5,
#       scales=list(cex=0.01)
#       )

xyplot(meanSpd_Day~meanSpd_Night, data=normVevent)

tmp.events <- events.summary %>% select(position, meanSpd_Night, meanSpd_Day)
tmp.nonEvents <- nonevents.summary %>% select(position, meanSpd_Night, meanSpd_Day)

eventVnon_percent <- 100 - (tmp.events/tmp.nonEvents) * 100
eventVnon_percent$position <- tmp.events$position

eventPrecip <- events.summary %>% select(position, meanPrecip_Night, meanPrecip_Day)
eventVnon_percent <- eventVnon_percent %>% left_join(eventPrecip, by='position')

xyplot(meanPrecip_Night~meanSpd_Night, data=eventVnon_percent, pch=16,
       main='Percent Speed Reduction from Normal at Night',
       xlab='Percent Speed Reduction From Normal', ylab='Mean Precipitation (mm/hr)',
       xlim=c(0,70), ylim=c(0,70),
       scales=list(x=list(at=seq(0,70,5)), y=list(at=seq(0,70,10))))

xyplot(meanSpd_Night~position, data=eventVnon_percent, pch=16)

smoothScatter(eventVnon_percent$meanSpd_Night, eventVnon_percent$meanPrecip_Night)

densityplot(~meanSpd_Night, data=eventVnon_percent, pch=16,
            main='Density Estimation of Speed Reduction from Normal at Night',
            xlab='Percent Speed Reduction From Normal',
            xlim=c(0,80), ylim=c(0,.1),
            scales=list(x=list(at=seq(0,80,5)), y=list(at=seq(0,.1,0.01))))


xyplot(meanPrecip_Day~meanSpd_Day, data=eventVnon_percent, pch=16,
       main='Percent Speed Reduction from Normal during Day',
       xlab='Percent Speed Reduction From Normal', ylab='Mean Precipitation (mm/hr)',
       xlim=c(0,70), ylim=c(0,70),
       scales=list(x=list(at=seq(0,70,5)), y=list(at=seq(0,70,10))))

densityplot(~meanSpd_Day, data=eventVnon_percent, pch=16,
            main='Density Estimation of Speed Reduction from Normal during Day',
            xlab='Percent Speed Reduction From Normal',
            xlim=c(0,80), ylim=c(0,.1),
            scales=list(x=list(at=seq(0,80,5)), y=list(at=seq(0,.1,0.01))))

smoothScatter(eventVnon_percent$meanSpd_Day, eventVnon_percent$meanPrecip_Day)


plot(events.summary$position, events.summary$sumNight, col='blue', pch=16)
points(events.summary$position, events.summary$sumDay, col='red', pch=16)

plot(events.summary$position, events.summary$meanSpd_Night, col='blue', pch=16)
points(events.summary$position, events.summary$meanSpd_Day, col='red', pch=16)

plot(events.summary$position, events.summary$meanPrecip_Night, col='blue', pch=16)
points(events.summary$position, events.summary$meanPrecip_Day, col='red', pch=16)

plot(events.summary$position, events.summary$sdPrecip_Night, col='blue', pch=16)
points(events.summary$position, events.summary$sdPrecip_Day, col='red', pch=16)


tmp <- traffic %>% select(position, lat, lon, startmm, endmm) %>% unique() %>% 
  right_join(eventVnon_percent, by='position')

tmp <- st_as_sf(tmp, coords=c('lon','lat'))
tmap_mode('view')
tm_shape(tmp) + tm_dots('meanSpd_Day')




# need another way to summarize the data
tmp2 <- traffic.nc %>% group_by(position) %>% 
  select(tstamp, speed, daylight, event)

tmp2 %>% ifelse(event==T, '')




