library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
library(hexbin)
library(hextri)
library(ggplot2); library(ggforce); library(ggridges);
library(sf)
library(tmap)

traffic <- fread('./data/processed/I-65N_WxSpd.csv')
#traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')
# traffic$day <- mday(traffic$tstamp)
anyNA(traffic$precip)

road.map <- traffic %>% select(startmm,lon,lat) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()

traffic <- traffic %>% mutate(event=ifelse(precip > 0, yes=T, no=F))
segs <- traffic %>% select(position) %>% unique()

# MAY NEED TO REMOVE LATER BUT STRIP OUT 20s
traffic <- traffic %>% filter(score == 30)

# pdf('./figures/precipVspeed.pdf')
# xyplot(precip ~ speed | factor(position), data=traffic, pch=16, alpha=0.2,
#        layout=c(3,3))
# dev.off()

# add urban vs rural
traffic <- traffic %>% mutate(urban=ifelse(position >= 2 & position <= 15,
                                           yes='Louisville', 
                                           no=ifelse(position >= 189 & position <= 239,
                                                     yes='Indianapolis',
                                                     no=ifelse(position >= 440 & position <= 458,
                                                               yes='Northern Indiana',
                                                               no='Rural')
                                           )
)
)
traffic$construction <- as.factor(traffic$construction)
levels(traffic$construction) <- c('Non-Construction','Construction')
traffic$dayofweek <- factor(traffic$dayofweek, levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
levels(traffic$dayofweek)

traffic$weekend <- ifelse(traffic$dayofweek == 'Saturday' | traffic$dayofweek == 'Sunday',
                          yes='Weekend', no='Weekday')
traffic$weekend <- as.factor(traffic$weekend)

# traffic$hours <- as.factor(traffic$hours)
# levels(traffic$hours)

traffic <- traffic %>% mutate(hour.range=ifelse(traffic$hours >= 4 & traffic$hours < 10,
                                           yes='Morning', 
                                           no=ifelse(traffic$hours >= 10 & traffic$hours < 16,
                                                     yes='Morning Rush',
                                                     no=ifelse(traffic$hours >= 15 & traffic$hours < 22,
                                                               yes='Afternoon Rush',
                                                               no='Evening')
                                           )
)
)
traffic$hour.range <- as.factor(traffic$hour.range)
levels(traffic$hour.range) <- c('Morning', 'Morning Rush', 'Afternoon Rush', 'Evening')

precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
#precip.ranges <- c(0,0.01,2.5,5,10,150)
#c(0,0.01,2.5,5,10,20,40,60,80,100,140) #old ranges
traffic$rcat <- cut(traffic$precip,
                    breaks=precip.ranges, right=F, include.lowest=T)


# drop out known crashes
c1 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-08 20:38:00' |
                                   .$tstamp == '2018-06-08 20:40:00' |
                                   .$tstamp == '2018-06-08 20:42:00' |
                                   .$tstamp == '2018-06-08 20:44:00' |
                                   .$tstamp == '2018-06-08 20:46:00' |
                                   .$tstamp == '2018-06-08 20:48:00' |
                                   .$tstamp == '2018-06-08 20:50:00' |
                                   .$tstamp == '2018-06-08 20:52:00' |
                                   .$tstamp == '2018-06-08 20:54:00' |
                                   .$tstamp == '2018-06-08 20:56:00' |
                                   .$tstamp == '2018-06-08 20:58:00' |
                                   .$tstamp == '2018-06-08 21:00:00' |
                                   .$tstamp == '2018-06-08 21:02:00' |
                                   .$tstamp == '2018-06-08 21:04:00' |
                                   .$tstamp == '2018-06-08 21:06:00' |
                                   .$tstamp == '2018-06-08 21:08:00') & 
                                  (.$startmm>=100 & .$startmm<=110)
                                 )
                       )
   ) %>% .$x
sub <- traffic[c1,]

c2 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-10 19:30:00' |
                                          .$tstamp == '2018-06-10 19:32:00' |
                                          .$tstamp == '2018-06-10 19:34:00' |
                                          .$tstamp == '2018-06-10 19:36:00' |
                                          .$tstamp == '2018-06-10 19:38:00' |
                                          .$tstamp == '2018-06-10 19:40:00' |
                                          .$tstamp == '2018-06-10 19:42:00' |
                                          .$tstamp == '2018-06-10 19:44:00' |
                                          .$tstamp == '2018-06-10 19:46:00' |
                                          .$tstamp == '2018-06-10 19:48:00' |
                                          .$tstamp == '2018-06-10 19:50:00' |
                                          .$tstamp == '2018-06-10 19:52:00' |
                                          .$tstamp == '2018-06-10 19:54:00' |
                                          .$tstamp == '2018-06-10 19:56:00' |
                                          .$tstamp == '2018-06-10 19:58:00' |
                                          .$tstamp == '2018-06-10 20:00:00') & 
                                         (.$startmm>=56 & .$startmm<=66)
                                       )
                             )
         ) %>% .$x

c3 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-12 06:00:00' |
                                          .$tstamp == '2018-06-12 06:02:00' |
                                          .$tstamp == '2018-06-12 06:04:00' |
                                          .$tstamp == '2018-06-12 06:06:00' |
                                          .$tstamp == '2018-06-12 06:08:00' |
                                          .$tstamp == '2018-06-12 06:10:00' |
                                          .$tstamp == '2018-06-12 06:12:00' |
                                          .$tstamp == '2018-06-12 06:14:00' |
                                          .$tstamp == '2018-06-12 06:16:00' |
                                          .$tstamp == '2018-06-12 06:18:00' |
                                          .$tstamp == '2018-06-12 06:20:00' |
                                          .$tstamp == '2018-06-12 06:22:00' |
                                          .$tstamp == '2018-06-12 06:24:00' |
                                          .$tstamp == '2018-06-12 06:26:00' |
                                          .$tstamp == '2018-06-12 06:28:00' |
                                          .$tstamp == '2018-06-12 06:30:00') & 
                                         (.$startmm>=62 & .$startmm<=72)
                                       )
                             )
         ) %>% .$x

c4 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-21 18:50:00' |
                                          .$tstamp == '2018-06-21 18:52:00' |
                                          .$tstamp == '2018-06-21 18:54:00' |
                                          .$tstamp == '2018-06-21 18:56:00' |
                                          .$tstamp == '2018-06-21 18:58:00' |
                                          .$tstamp == '2018-06-21 19:00:00' |
                                          .$tstamp == '2018-06-21 19:02:00' |
                                          .$tstamp == '2018-06-21 19:04:00' |
                                          .$tstamp == '2018-06-21 19:06:00' |
                                          .$tstamp == '2018-06-21 19:08:00' |
                                          .$tstamp == '2018-06-21 19:10:00' |
                                          .$tstamp == '2018-06-21 19:12:00' |
                                          .$tstamp == '2018-06-21 19:14:00' |
                                          .$tstamp == '2018-06-21 19:16:00' |
                                          .$tstamp == '2018-06-21 19:18:00' |
                                          .$tstamp == '2018-06-21 19:20:00') & 
                                         (.$startmm>=128 & .$startmm<=138)
                                       )
                             )
         ) %>% .$x

c5 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-21 17:30:00' |
                                          .$tstamp == '2018-06-21 17:32:00' |
                                          .$tstamp == '2018-06-21 17:34:00' |
                                          .$tstamp == '2018-06-21 17:36:00' |
                                          .$tstamp == '2018-06-21 17:38:00' |
                                          .$tstamp == '2018-06-21 17:40:00' |
                                          .$tstamp == '2018-06-21 17:42:00' |
                                          .$tstamp == '2018-06-21 17:44:00' |
                                          .$tstamp == '2018-06-21 17:46:00' |
                                          .$tstamp == '2018-06-21 17:48:00' |
                                          .$tstamp == '2018-06-21 17:50:00' |
                                          .$tstamp == '2018-06-21 17:52:00' |
                                          .$tstamp == '2018-06-21 17:54:00' |
                                          .$tstamp == '2018-06-21 17:56:00' |
                                          .$tstamp == '2018-06-21 17:58:00' |
                                          .$tstamp == '2018-06-21 18:00:00') & 
                                         (.$startmm>=172 & .$startmm<=182)
                                       )
                             )
         ) %>% .$x

c6 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-26 14:50:00' |
                                          .$tstamp == '2018-06-26 14:52:00' |
                                          .$tstamp == '2018-06-26 14:54:00' |
                                          .$tstamp == '2018-06-26 14:56:00' |
                                          .$tstamp == '2018-06-26 14:58:00' |
                                          .$tstamp == '2018-06-26 15:00:00' |
                                          .$tstamp == '2018-06-26 15:02:00' |
                                          .$tstamp == '2018-06-26 15:04:00' |
                                          .$tstamp == '2018-06-26 15:06:00' |
                                          .$tstamp == '2018-06-26 15:08:00' |
                                          .$tstamp == '2018-06-26 15:10:00' |
                                          .$tstamp == '2018-06-26 15:12:00' |
                                          .$tstamp == '2018-06-26 15:14:00' |
                                          .$tstamp == '2018-06-26 15:16:00' |
                                          .$tstamp == '2018-06-26 15:18:00' |
                                          .$tstamp == '2018-06-26 15:20:00') & 
                                         (.$startmm>=110 & .$startmm<=120)
                                       )
                             )
         ) %>% .$x

all_crashes <- c(c1,c2,c3,c4,c5,c6) %>% unique()
traffic <- traffic[-all_crashes,]

sum(traffic$precip > 0)/nrow(traffic) * 100


sub <- traffic[sample(nrow(traffic),10000),]

xyplot(startmm~speed | factor(dayofweek)*factor(hour.range)*factor(construction),
       data=sub, groups=event, pch=16, alpha=0.3, layout=c(7,1,1))

distro <- traffic
distro$event <- ifelse(distro$event==T, yes='Precipitable', no='Non-Precipitable')

pdf('./figures/bulkDensity.pdf', width=12, height=8)
densityplot(~speed | factor(weekend)*factor(hour.range)*factor(construction)*factor(urban),
            data=distro, groups=event, 
            layout=c(8,2), xlab='Speed (mph)',
            axis=axis.grid, alhpa=0.5,
            from=0, to=90, plot.points=F, auto.key=T)
dev.off()

pdf('./figures/bulkECDF.pdf', width=12, height=8)
ecdfplot(~speed | factor(weekend)*factor(hour.range)*factor(construction)*factor(urban),
         data=distro, groups=event, 
         layout=c(8,2), xlab='Speed (mph)',
         axis=axis.grid, alhpa=0.5,
         from=0, to=90, plot.points=F, auto.key=T)
dev.off()

pdf('./figures/bulkbwplot.pdf', width=12, height=8)
bwplot(rcat~speed | factor(weekend)*factor(hour.range)*factor(construction)*factor(urban), 
       data=traffic, axis=axis.grid, do.out=F, 
       xlab='Speed (mph)', ylab='Precipitation Rate (mm/hr)',
       cex=.2, layout=c(8,2))
dev.off()

test <- traffic %>% group_by(event, rcat, weekend, hour.range, construction, urban) %>% 
  summarize(n=n(),
            avgSpd=mean(speed),
            medianSpd=median(speed))

pdf('./figures/avgSpd_Bylocation.pdf', width=10, height=7)
xyplot(rcat~avgSpd | factor(hour.range)*factor(construction)*factor(weekend), data=test,
       groups=urban, auto.key=T, cex=0.5, alpha=0.5,
       axis=axis.grid, ylab='Precipitation Intensity (mm/hr)',
       xlab='Average Speed (mph)',
       layout=c(4,2), xlim=c(40,80),
       scales=list(x=list(at=seq(35,85,5))))
dev.off()

pdf('./figures/medianSpd_Bylocation.pdf', width=10, height=7)
xyplot(rcat~medianSpd | factor(hour.range)*factor(construction)*factor(weekend), data=test,
       groups=urban, auto.key=T, cex=0.5, alpha=0.5,
       axis=axis.grid, ylab='Precipitation Intensity (mm/hr)',
       xlab='Average Speed (mph)',
       layout=c(4,2), xlim=c(40,80),
       scales=list(x=list(at=seq(35,85,5))))
dev.off()

# recalculate the above figures as % reductions
















# pull out just the Indy region
indy <- sub %>% filter(urban=='Rural')

xyplot(startmm~speed | factor(dayofweek)*factor(hours)*factor(construction),
       data=indy, groups=event, pch=16, alpha=0.3, layout=c(5,1,1))



pdf('./figures/positionVspeed_hours.pdf', width=10, height=8)

xyplot(startmm ~ speed | factor(dayofweek)*factor(hours)*factor(construction), 
       data=sub, ylab='Road Position', xlab='Speed (mph)', par.strip.text=list(cex=1),
       par.settings=list(layout.heights=list(strip=1)), groups=event,
       pch=16, alpha=0.2, grid=T, layout=c(3,2,1), cex=.5)

xyplot(speed ~ startmm | factor(dayofweek)*factor(hours)*factor(construction), 
       data=traffic, ylab='Road Position', xlab='Speed (mph)', par.strip.text=list(cex=1),
       par.settings=list(layout.heights=list(strip=1)), groups=event,
       pch=16, alpha=0.2, grid=T, layout=c(2,1), cex=.5)

dev.off()

hexbinplot(startmm ~ speed | factor(dayofweek)*factor(event)*factor(construction),
           data=sub, ylab='Road Position', xlab='Speed (mph)', bins=50,
           aspect=1, layout=c(7,2))


levelplot(speed~as.integer(tstamp)*startmm, data=sub)
colorRampPalette()

levelplot(speed~tstamp*startmm | factor(dayofweek)*factor(construction), data=sub, 
          aspect=1, 
          col.regions=colorRampPalette(c("pink", "red", "orange","blue", "green"))
)















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










# produce levels indicating precip levels for bw plots
tmp <- traffic


#tmp$construction <- as.factor(tmp$construction)
#levels(tmp$construction) <- c('Non-Construction','Construction')



sub <- tmp[sample(nrow(tmp),10000),]



# levelplot(speed~position*hours | factor(dayofweek), data=tmp)

traffic.summary.r <- tmp %>% group_by(hour.range,weekend,urban,construction,rcat) %>% 
  summarise(min.r=min(speed), max.r=max(speed), median.r=median(speed), std.r=sd(speed), n.r=n())

traffic.baseline <- tmp %>% filter(precip == 0) %>% 
  group_by(hour.range,weekend,urban,construction,rcat) %>% 
  summarise(min.b=min(speed), max.b=max(speed), median.b=median(speed), std.b=sd(speed), n.b=n())

traffic.summary <- left_join(traffic.summary.r, traffic.baseline)
traffic.summary <- zoo::na.locf(traffic.summary, fromLast=T)
traffic.summary[seq(405,415),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[404,c('min.b','max.b','median.b', 'std.b', 'n.b')]

# NOTE: THESE FILL IN LINES WILL HAVE TO CHANGE IF ANY NEW STATS ARE ADDED OR REGIONS.
# below was a highly manual method for filling NAs
# traffic.summary[seq(2,12),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[1,c('min.b','max.b','median.b', 'std.b', 'n.b')]


percent.reduction <- 100 - (traffic.summary$median.r / traffic.summary$median.b)*100
traffic.summary$median.percent.reduction <- percent.reduction %>% round(2)



xyplot(rcat~median.percent.reduction | factor(weekend)*factor(hour.range)*factor(construction),
       data=traffic.summary, groups=urban, pch=16, alpha=0.8,
       axis=axis.grid, auto.key=T,
       layout=c(4,2))

testit <- traffic.summary %>% filter(n.r > 100)
xyplot(rcat~median.percent.reduction | factor(weekend)*factor(hour.range)*factor(construction),
       data=testit, groups=urban, pch=16, alpha=0.5,
       axis=axis.grid, auto.key=T,
       layout=c(4,2))



bwplot(percent.reduction~construction, data=traffic.summary,groups=construction)


# graphical method for presenting the tabular percent reduction data.
dotplot(rcat~percent.reduction | factor(construction), data=traffic.summary, groups=urban, auto.key=T,
        pch=16, xlab='Percent Reduction from Normal', layout=c(2,1),
        ylab='Precipitation Range (mm/hr)', alpha=0.5, xlim=c(-1,13),
        axis=axis.grid, scales=list(x=list(at=seq(-2,14,2))))

tmp2 <- traffic[seq(1,10000),]

pdf('./figures/positionVspeed_hours.pdf', width=10, height=8)
xyplot(position ~ speed | factor(hours)*factor(construction), data=traffic, 
       ylab='Road Position', xlab='Speed (mph)', par.strip.text=list(cex=.5),
       pch=16, alpha=0.2, group=event, auto.key=T, layout=c(12,1,4),)
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




