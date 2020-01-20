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
  group_by(position) %>% do(zoo::na.locf(.)) %>% filter((minute(tstamp)%%2)==0) %>% 
  left_join(precip, by=c('tstamp', 'lon', 'lat', 'position', 'xdid', 'direction'))


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
xyplot(speed ~ precip | factor(position), data=traffic, layout=c(3,3))
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
traffic$construction <- ifelse(tmp.tmp$startmm >= 8 & tmp.tmp$startmm <= 16 |
                                 tmp.tmp$startmm >= 50 & tmp.tmp$startmm <= 68 |
                                 tmp.tmp$startmm >= 141 & tmp.tmp$startmm <= 165 |
                                 tmp.tmp$startmm >= 167 & tmp.tmp$startmm <= 176 |
                                 tmp.tmp$startmm >= 197 & tmp.tmp$startmm <= 207 |
                                 tmp.tmp$startmm >= 229 & tmp.tmp$startmm <= 253,
                               yes=T, no=F)
# remove construction
traffic.nc <- traffic %>% filter(construction==F)


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




