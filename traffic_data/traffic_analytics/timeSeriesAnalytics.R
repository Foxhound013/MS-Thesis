library(data.table)
library(dplyr)
library(lattice)

traffic <- fread('./data/processed/I-65N_WxSpd.csv')
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')
anyNA(traffic$precip)
# order by tstamp
traffic <- traffic %>% group_by(position)
traffic <- traffic[do.call(order,traffic),]

road.map <- traffic %>% select(position,lon,lat,startmm,endmm) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()


# prototype off of one known event
tmp <- traffic %>% filter(mday(tstamp) == 9 & position == 238 & hours>=16 & hours<19)
# 4 minute moving average to calculate major speed drops.
tmp <- tmp %>% mutate(avgSpd=frollmean(x=tmp$speed,n=2,fill=NA))

xyplot(speed+avgSpd~tstamp, data=tmp, type=c('p','l','g'), auto.key=T)
xyplot(precip~tstamp, data=tmp, type=c('p','l','g'))

# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 8, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)
xyplot(speed+avgSpd~tstamp, data=tmp, type=c('p','l','g'), auto.key=T, groups=event)

# window building 

eventRow <- which(tmp$event == T) # tells which row is an event

eventWindow <- tmp[seq(eventRow-15,eventRow+15,1),]

xyplot(speed+avgSpd~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)

# works well, you'll need to loop over the eventRows for larger datasets as there will
# certainly be multiple events that crop up as time goes on.

# delta speed: locate the speed just prior to percent reduction being met - min actual speed for window.
eventLocation <- which(eventWindow$event==T)
ds <- eventWindow[eventLocation-1,'avgSpd'][[1]] - min(eventWindow$speed)



# maximum precip: The maximum precipitation in the window time frame
max_precip <- max(eventWindow$precip)
max_precip.tstamp <- eventWindow[which(eventWindow$precip == max_precip), 'tstamp']



# time to recovery: time at which speed goes above the avg speed from the time when the event T tag occurred
# minus the time at which the T event tag occurred.
start.tstamp <- eventWindow[eventLocation,'tstamp'][[1]]
start.avgSpd <- eventWindow[eventLocation, 'avgSpd'][[1]] # this is the threshold for recovery
# perhaps the recovery theshold should use the time step before spd.

tmp2 <- eventWindow[as.integer(rownames(eventWindow))>=eventLocation,]
end.tstamp <- tmp2[tmp2$speed >= start.avgSpd,]$tstamp[1] # time of recovery (if not possible, produces NA)

t2r <- difftime(end.tstamp,start.tstamp) %>% as.integer()
is.na(t2r)



# Time to Impact: time when speed event = T minus time before when rain > 0
start.precip <- eventWindow[eventWindow$precip>0,]$tstamp[1]
t2i <- difftime(start.tstamp, start.precip) %>% as.integer()




# basic stats for a single one off event are calculated above, but what about when there are multiples
# in a longer time frame. Below is the prototype for automating this across a longer time period.








