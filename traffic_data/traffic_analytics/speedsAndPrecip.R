library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
library(hexbin)
library(ggplot2); library(ggforce);
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
traffic$weekend <- factor(traffic$weekend, levels=c('Weekday', 'Weekend'))
levels(traffic$weekend)

#traffic <- traffic %>% filter(weekend=='Weekday')

# drop out known crashes
# sub <- traffic %>% f
# sub <- traffic %>% filter(day!= 8 & hours!=20 & (mins != 38 | mins != 40) & (startmm < 100 | startmm > 110))


sub <- traffic[sample(nrow(traffic),10000),]

xyplot(startmm~speed | factor(dayofweek)*factor(hours)*factor(construction),
       data=sub, groups=event, pch=16, alpha=0.3, layout=c(7,1,1))


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

myPanel <- function(x,y) {
  panel.grid(v = 2)
  panel.xyplot(x, y, pch=16)
  panel.loess(x, y, span = .01, col='red')
}


xyplot(startmm~speed | factor(hours), data=sub, groups=event, 
       pch=16, layout=c(6,1,1),
)


speed.shingle <- equal.count(sub$speed, number=10, overlap=0)
attributes(speed.shingle)
levels(speed.shingle)

spd.cut <- cut(traffic$speed, breaks=c(0,20,40,60,80,100))
spd.cut.sub <- cut(sub$speed, breaks=c(0,20,40,60,80,100))

pdf('./figures/positionVspeed_bySpeed.pdf')


xyplot(startmm~speed | spd.cut.sub*factor(dayofweek)*factor(hours)*factor(construction),
       data=sub, groups=event,
       pch=16, alpha=0.2,
       scales=list(x=list(relation='free')),
       layout=c(5,1,1),
       prepanel=function(x,y){
         
         #lims=c(min(x),max(x))
         if (length(x) == 0) {
           return()
         } else {
           if (x <= 20) {
             xlim=c(0,20)
             scales=list(x=list(at=seq(0,20,5)))
           } else if (x > 20 & x <= 40) {
             xlim=c(20,40)
             scales=list(x=list(at=seq(20,40,5)))
           } else if (x > 40 & x <= 60) {
             xlim=c(40,60)
             scales=list(x=list(at=seq(40,60,5)))
           } else if (x > 60 & x <= 80) {
             xlim=c(60,80)
             scales=list(x=list(at=seq(60,80,5)))
           } else if (x > 80) {
             xlim=c(80,100)
             scales=list(x=list(at=seq(80,100,5)))
           }
         }
         
         
         #lims=c(ifelse(length(x)>0, yes=min(x), no=NA), ifelse(length(x)>0, yes=max(x), no=NA))
         #list(xlim=lims)
       }
)


dev.off()

spd.cut.sub <- cut(sub$speed, breaks=c(0,10,20,30,40,50,
                                       51,52,53,54,55,56,57,58,59,
                                       60,61,62,63,64,65,66,67,68,69,
                                       70,71,72,73,74,75,76,77,78,79,
                                       80,100))
pdf('./figures/positionVspeed_stripplot.pdf')
stripplot(position~spd.cut.sub | factor(dayofweek)*factor(hours)*factor(construction), 
          data=sub,
          jitter.data=T, groups=event,
          pch=16, alpha=0.2,
          layout=c(1,1),
          scales=list(x=list(rot=90)))
dev.off()


xyplot(speed~startmm | factor(dayofweek)*factor(hours)*factor(construction), 
       data=sub,
       groups=event,
       layout=c(1,2,1))

test <- traffic %>% group_by(startmm, dayofweek, hours, construction, event) %>% 
  summarize(avgSpd=mean(speed, na.rm=T))

xyplot(avgSpd~startmm | factor(dayofweek)*factor(hours)*factor(construction),
       data=test, groups=event,
       pch=16, alpha=0.4,
       layout=c(7,1)
)

levelplot(avgSpd~factor(event)*startmm | factor(dayofweek)*factor(hours)*factor(construction), 
          data=test, layout=c(7,1,1),
          scales=list(x=list(rot=90)),
          col.regions=colorRampPalette(c("pink", "red", "orange", "green")),
          ylim=c(0,270)
)


pdf('./figures/levplot_test.pdf')
levelplot(avgSpd~factor(event)*startmm | factor(dayofweek)*factor(hours)*factor(construction), 
          data=test, layout=c(7,2,1),
          scales=list(x=list(rot=90)),
          col.regions=colorRampPalette(c("pink", "red", "orange", "green")),
          ylim=c(0,270)
)
dev.off()

pdf('./figures/loessPlot_Events.pdf')
xyplot(speed~startmm | factor(dayofweek)*factor(hours)*factor(construction),
       data=traffic, layout=c(7,2,1), groups=event, auto.key=T,
       pch=16, alpha=0.5,
       type=c('smooth'), span=2/3, degree=1
)
dev.off()

pdf('./figures/positionVspeed_bySpeed.pdf')
xyplot(startmm~speed | factor(dayofweek)*factor(hours)*factor(construction),
       data=traffic, layout=c(7,1,1), groups=event, auto.key=T,
       pch=16, alpha=0.2)
dev.off()







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


# only defining the event this way here for density plots, redefined later.

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
precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
tmp$rcat <- cut(tmp$precip, breaks=precip.ranges, right=F, include.lowest=T)
# tmp$rcat <- cut(tmp$precip, breaks=c(-1,0,0.01,10,20,30,40,50,60,70,80,90,100,140), right=F, include.lowest=T)
tmp$section <- cut(tmp$position, breaks=c(0,100,200,300,400,500), right=F, include.lowest=T)

#tmp$construction <- as.factor(tmp$construction)
#levels(tmp$construction) <- c('Non-Construction','Construction')

pdf('./figures/bwplot_speedByCat.pdf')
bp <- bwplot(rcat~speed | factor(section)*factor(construction), data=tmp, 
             ylab='Precip. Range (mm/hr)', xlab='Speed (mph)', 
             do.out=F, layout=c(1,5,2), cex=.3, scales=list(cex=.5),
             par.strip.text=list(cex=.7))
dev.off()

sub <- tmp[sample(nrow(tmp),10000),]

# consider factoring by daylight as well
pdf('./figures/bwplot_speedByCat&urban&others.pdf', width=10, height=7)
bwplot(rcat~speed | factor(urban)*factor(construction)*factor(hours)*factor(dayofweek), 
       data=tmp, layout=c(1,4),
       ylab='Precipitation Range (mm/hr)', xlab='Speed (mph)',
       do.out=F, cex=.3, axis=axis.grid, xlim=c(0,90),
       scales=list(cex=.5, x=list(at=seq(0,90,5))),
       par.strip.text=list(cex=.7), coef=1.5)
dev.off()

bwplot(rcat~speed | factor(urban)*factor(construction)*factor(dayofweek)*factor(hours), 
       data=tmp, layout=c(1,4),
       ylab='Precipitation Range (mm/hr)', xlab='Speed (mph)',
       do.out=F, cex=.3, axis=axis.grid, xlim=c(0,90),
       scales=list(cex=.5, x=list(at=seq(0,90,5))),
       par.strip.text=list(cex=.7), coef=1.5)

# levelplot(speed~position*hours | factor(dayofweek), data=tmp)

traffic.summary.r <- tmp %>% group_by(urban,construction,rcat) %>% 
  summarise(min.r=min(speed), max.r=max(speed), median.r=median(speed), std.r=sd(speed), n.r=n())

traffic.baseline <- tmp %>% filter(precip == 0) %>% group_by(urban,construction,rcat) %>% 
  summarise(min.b=min(speed), max.b=max(speed), median.b=median(speed), std.b=sd(speed), n.b=n())

traffic.summary <- left_join(traffic.summary.r, traffic.baseline, )
# NOTE: THESE FILL IN LINES WILL HAVE TO CHANGE IF ANY NEW STATS ARE ADDED OR REGIONS.
traffic.summary[seq(2,12),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[1,c('min.b','max.b','median.b', 'std.b', 'n.b')]
traffic.summary[seq(14,21),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[13,c('min.b','max.b','median.b', 'std.b', 'n.b')]
traffic.summary[seq(23,30),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[22,c('min.b','max.b','median.b', 'std.b', 'n.b')]
traffic.summary[seq(32,39),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[31,c('min.b','max.b','median.b', 'std.b', 'n.b')]
traffic.summary[seq(41,51),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[40,c('min.b','max.b','median.b', 'std.b', 'n.b')]
traffic.summary[seq(53,63),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[52,c('min.b','max.b','median.b', 'std.b', 'n.b')]


percent.reduction <- 100 - (traffic.summary$median.r / traffic.summary$median.b)*100
traffic.summary$percent.reduction <- percent.reduction %>% round(2)

bwplot(percent.reduction~construction, data=traffic.summary,groups=construction)
levelplot(percent.reduction~construction*urban, data=traffic.summary)

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




