library(data.table)
library(dplyr)
library(lattice)
library(sf); library(tmap);

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

xyplot(speed+avgSpd+precip~tstamp, data=tmp, type=c('p','l','g'), auto.key=T)

# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 10, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)

xyplot(speed+avgSpd~tstamp, data=tmp, type=c('p','l','g'), auto.key=T, groups=event)

# window building 

eventRow <- which(tmp$event == T) # tells which row is an event
eventWindow <- tmp[seq(eventRow[1]-20,eventRow[1]+20,1),] # set to take the first from a sequence
eventCenter <- 21
xyplot(speed+avgSpd~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)

# works well, you'll need to loop over the eventRows for larger datasets as there will
# certainly be multiple events that crop up as time goes on.

# If precip occured in the last 30 minutes, move to next event, else construct stats
# WARNING: HARD CODED VALUE SET TO THE SYMETRIC EVENT CENTER, 16 WILL NOT ALWAYS BE TRUE IF WINDOW MODIFIED
precipPresent <- ifelse(sum(eventWindow[seq(1,eventCenter),'precip']) > 0, yes=T, no=F)

# now would be the time to say if precip present, generate stats, else go to next eventRow.

# delta speed: locate the speed just prior to percent reduction being met - min actual speed for window.
#eventLocation <- which(eventWindow$event==T)
ds <- eventWindow[eventCenter-1,'avgSpd'][[1]] - min(eventWindow$speed)



# maximum precip: The maximum precipitation in the window time frame
max_precip <- max(eventWindow$precip)
max_precip.tstamp <- eventWindow[which(eventWindow$precip == max_precip), 'tstamp'][[1]]



# time to recovery: time at which speed goes above the avg speed from the time when the event T tag occurred
# minus the time at which the T event tag occurred.
start.tstamp <- eventWindow[eventCenter,'tstamp'][[1]]
start.avgSpd <- eventWindow[eventCenter, 'avgSpd'][[1]] # this is the threshold for recovery
# perhaps the recovery theshold should use the time step before spd.

tmp2 <- eventWindow[as.integer(rownames(eventWindow))>=eventCenter,]
end.tstamp <- tmp2[tmp2$speed >= start.avgSpd,]$tstamp[1] # time of recovery (if not possible, produces NA)

t2r <- difftime(end.tstamp,start.tstamp) %>% as.integer()
is.na(t2r)



# Time to Impact: time when speed event = T minus time before when rain > 0
start.precip <- eventWindow[eventWindow$precip>0,]$tstamp[1]
t2i <- difftime(start.tstamp, start.precip) %>% as.integer()








# basic stats for a single one off event are calculated above, but what about when there are multiples
# in a longer time frame. Below is the prototype for automating this across a longer time period.
# this prototype will limit the work to a single segment before finalizing and attempting on all.

# 4 minute moving average to calculate major speed drops.
tmp <- traffic %>% filter(position == 238) %>% mutate(avgSpd=frollmean(x=speed,n=2,fill=NA))

# ideally, this would get split up into more cut and stack plots
xyplot(speed+avgSpd+precip~tstamp, data=tmp, type=c('p','l','g'), auto.key=T)

# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 10, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)
xyplot(speed+avgSpd+precip~tstamp, data=tmp, type=c('p','l','g'), auto.key=T, groups=event)


# window building 
eventRow <- which(tmp$event == T) # tells which row is an event

# container for stats
# segment.stats <- data.frame(segment=NA,
#                             ds=NA,
#                             max_precip=NA,
#                             max_precip.tstamp=NA,
#                             event.start=NA,
#                             recovery.speed=NA,
#                             recovery.tstamp=NA,
#                             t2r=NA,
#                             precipStart.tstamp=NA,
#                             t2i=NA)

generate_stats <- function(segment, segData) {
  # window building 
  eventRow <- which(segData$event == T)
  
  segment.stats <- data.frame()
  eventCenter <- 21
  for (i in eventRow) {
    
    # verify that a full window can be constructed, if not move on.
    if(i <= 20 | i >= 21580) {
      next()
    }
    
    eventWindow <- tmp[seq(i-20,i+20),]
    precipPresent <- ifelse(sum(eventWindow[seq(1,eventCenter),'precip']) > 0, yes=T, no=next())
    
    # generate stats
    # Delta Speed (ds)
    ds <- eventWindow[eventCenter-1,'avgSpd'][[1]] - min(eventWindow$speed)
    max_precip <- max(eventWindow$precip)
    max_precip.tstamp <- eventWindow[which(eventWindow$precip == max_precip), 'tstamp'][[1]]
    
    # Time to Recover (t2r)
    start.tstamp <- eventWindow[eventCenter,'tstamp'][[1]]
    start.avgSpd <- eventWindow[eventCenter, 'avgSpd'][[1]] # this is the threshold for recovery
    # perhaps the recovery theshold should use the time step before spd.
    
    tmp2 <- eventWindow[as.integer(rownames(eventWindow))>=eventCenter,]
    end.tstamp <- tmp2[tmp2$speed >= start.avgSpd,]$tstamp[1] # time of recovery (if not possible, produces NA)
    
    t2r <- difftime(end.tstamp,start.tstamp) %>% as.integer()
    is.na(t2r)
    
    # Time to Impact (t2i)
    start.precip <- eventWindow[eventWindow$precip>0,]$tstamp[1]
    t2i <- difftime(start.tstamp, start.precip) %>% as.integer()
    
    # record all stats in a single data frame
    tmp.stats <- data.frame(segment=segment,
                            eventRow=i,
                            ds=ds,
                            max_precip=max_precip,
                            max_precip.tstamp=max_precip.tstamp,
                            event.start=start.tstamp,
                            recovery.speed=start.avgSpd,
                            recovery.tstamp=end.tstamp,
                            t2r=t2r,
                            precipStart.tstamp=start.precip,
                            t2i=t2i)
    
    segment.stats <- bind_rows(segment.stats, tmp.stats)
    
    
    # setting a break to check stats of first calculated event
    # if(precipPresent) {
    #   # DIAGNOSTIC PLOT
    #   # xyplot(speed+avgSpd+precip~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)
    #   break
    # }
  }

  return(segment.stats)
}

tmp <- traffic %>% filter(position > 230 & position < 240) %>% 
  mutate(avgSpd=frollmean(x=speed,n=2,fill=NA))
tmp <- traffic %>% mutate(avgSpd=frollmean(x=speed,n=2,fill=NA))
# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 8, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)

uniqueSegs <- tmp %>% select(position) %>% unique()
uniqueSegs <- uniqueSegs[[1]]


segment.stats <- data.frame()
for (i in uniqueSegs) {
  tmp2 <- tmp %>% filter(position==i)
  segment.stats <- bind_rows(segment.stats, generate_stats(i, segData=tmp2))
}


segStats.tmp <- segment.stats %>% filter(max_precip >= 0)
xyplot(max_precip~ds, data=segStats.tmp, pch=16, alpha=0.2)

smoothScatter(segStats.tmp$ds, segStats.tmp$max_precip)

segStats.tmp$cluster <- kmeans(segStats.tmp$max_precip, centers=3)$cluster
xyplot(max_precip~ds, data=segStats.tmp, pch=16, alpha=0.2, groups=cluster, auto.key=T)

bwplot(cluster~t2r, data=segStats.tmp)
