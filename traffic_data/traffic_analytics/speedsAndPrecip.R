library(data.table)
library(dplyr)
library(lattice)
library(hexbin)

traffic <- fread('../../../../../Desktop/June_I-65_2018.csv',
                 col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                             'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
traffic <- traffic %>% filter(direction=='N')
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')

precip <- fread('../../../../../Desktop/6_out743.csv')
precip <- precip %>% filter(direction=='N')
precip$tstamp <- as.POSIXct(precip$tstamp, tz='utc', origin='1970-01-01 00:00:00')

# The traffic data is set to 1 minute resolution, scale down to 2 minute resolution
# in order to match the precip data
setDT(traffic)
traffic <- traffic[,list(speed=mean(speed)), 
                   by=list(xdid=xdid, 
                           position=position, 
                           tstamp=as.POSIXct(cut(tstamp, '2 min'),tz='UTC'),
                           lat=lat,
                           lon=lon,
                           roadname=roadname,
                           direction=direction,
                           bearing=bearing)]

traffic <- traffic %>% 
  left_join(precip, by=c('tstamp', 'lon', 'lat', 'position', 'xdid', 'direction'))

# locate missing values and impute
tmp <- traffic[is.na(traffic$precip),]
# impute missing values with most recent prior value
traffic <- traffic %>% group_by(position) %>% do(zoo::na.locf(.))
traffic <- traffic[order(traffic$tstamp),]
tmp <- traffic[is.na(traffic$precip),]


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

tmp <- kmeans(traffic$speed, 2)
tmp$tot.withinss
traffic$cluster <- tmp$cluster
pdf('./figures/precipVspeed_clust.pdf')
xyplot(speed ~ precip | factor(position), data=traffic, groups=cluster, 
       pch=16, layout=c(3,3))
dev.off()


# rather than use k means to determine thresholds, use 55 as a threshold
# this will be roughly 10% less than 65, a typical speed limit for most of I-65
# 55 is the lowest postest speed limit on I-65

traffic.slow <- traffic %>% filter(speed <= 55)
traffic.fast <- traffic %>% filter(speed > 55)

nrow(traffic.fast)/nrow(traffic) * 100
nrow(traffic.slow)/nrow(traffic) * 100

# slow speeds make up a very small percentage of the dataset

tmp <- traffic.slow %>% filter(position==250)
xyplot(speed~precip | factor(position), data=traffic.slow,
       layout=c(1,1,1))



# characterize the dataset via time slots
traffic$hours <- lubridate::hour(traffic$tstamp)
traffic$mins <- lubridate::minute(traffic$tstamp)

# daylight 1000 to 0115 UTC
traffic$daylight <- NA
traffic[which(((traffic$hours>=1 & traffic$mins>=15) | (traffic$hours > 1 & traffic$mins >=0)) & 
                (traffic$hours < 10) ),'daylight'] <- F
traffic[is.na(traffic$daylight),'daylight'] <- T

# tmp <- traffic %>% filter(position==250) # visual verification

traffic$dayofweek <- weekdays(traffic$tstamp) %>% 
  factor(levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))


# pdf('./figures/precipVspeed_dayofweek.pdf')
# xyplot(precip~speed | factor(dayofweek), data=traffic, pch=16, col='deepskyblue3', alpha=0.5,
#        layout=c(7,1), par.strip.text=list(cex=.8))
# dev.off()


# norm defined as greater than 55
traffic.events <- traffic %>% group_by(position) %>% 
  transmute(tstamp=tstamp,
            position=position,
            speed=speed,
            precip=precip,
            daylight=daylight,
            dayofweek=dayofweek,
            event=)


tmp <- traffic
tmp$event <- ifelse((tmp$precip>0 & tmp$speed <=55), yes=T, no=F)
sum(tmp[,'event']==T)






