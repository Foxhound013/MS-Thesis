library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
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
tmp <- traffic %>% filter(mday(tstamp) == 10 & position == 102 & hours>=21 & hours<=23)
tmp <- traffic %>% filter(position == 123)
# 4 minute moving average to calculate major speed drops.
tmp <- tmp %>% mutate(avgSpd=frollmean(x=tmp$speed,n=2,fill=NA))

xyplot(speed+avgSpd+precip~tstamp, data=tmp, type=c('p','l','g'), auto.key=T)

# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 10, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)

xyplot(speed+avgSpd~tstamp, data=tmp, type=c('p','l','g'), auto.key=T, groups=event)

# window building 

eventRow <- which(tmp$event == T) # tells which row is an event
# which(eventRow==7152)
eventWindow <- tmp[seq(eventRow[10]-15,eventRow[10]+15,1),] # set to take the first from a sequence
eventCenter <- 16
xyplot(speed+avgSpd+precip~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)

# works well, you'll need to loop over the eventRows for larger datasets as there will
# certainly be multiple events that crop up as time goes on.

# If precip occured in the last 30 minutes, move to next event, else construct stats
# WARNING: HARD CODED VALUE SET TO THE SYMETRIC EVENT CENTER, 16 WILL NOT ALWAYS BE TRUE IF WINDOW MODIFIED
precipPresent <- ifelse(sum(eventWindow[seq(1,eventCenter),'precip']>0) > 0, yes=T, no=F)

# now would be the time to say if precip present, generate stats, else go to next eventRow.

# delta speed: locate the speed just prior to percent reduction being met - min actual speed for window.
#eventLocation <- which(eventWindow$event==T)
ds <- eventWindow[eventCenter-1,'speed'][[1]] - min(eventWindow[seq(eventCenter,eventCenter+15),'speed'])


# maximum precip: The maximum precipitation in the window time frame
max_precip <- max(eventWindow$precip)
max_precip.tstamp <- eventWindow[which(eventWindow$precip == max_precip), 'tstamp'][[1]]



# time to recovery: time at which speed goes above the avg speed from the time when the event T tag occurred
# minus the time at which the T event tag occurred.
start.tstamp <- eventWindow[eventCenter,'tstamp'][[1]]
start.avgSpd <- eventWindow[eventCenter, 'avgSpd'][[1]] # this is the threshold for recovery
# perhaps the recovery theshold should use the time step before spd.

tmp2 <- eventWindow[as.integer(rownames(eventWindow))>=eventCenter,]
end.tstamp <- tmp2[tmp2$speed > start.avgSpd,]$tstamp[1] # time of recovery (if not possible, produces NA)

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
  # segData <- tmp2
  # window building 
  eventRow <- which(segData$event == T)
  
  segment.stats <- data.frame()
  eventCenter <- 16
  prevEvent <- 0
  # i <- eventRow[1]
  # if(segment==122){
  #   browser()
  # }
  # step through each event in the 
  for (i in eventRow) {
    # verify that a full window can be constructed, if not move on.
    if(i <= 15 | i >= 21585) {
      next
    }
    # remove events that are too close in time to one another
    if(prevEvent+15 > i) {
      next
    }
    
    eventWindow <- segData[seq(i-15,i+15),]
    precipPresent <- ifelse(sum(eventWindow[seq(1,eventCenter),'precip']>0) > 0, yes=T, no=next)
    
    # png(paste0('./figures/automatedEventFigs/',segment,'_',i,'.png'))
    # p <- xyplot(speed+avgSpd+precip~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)
    # print(p)
    # dev.off()
    
    # generate stats
    # Delta Speed (ds)
    ds <- eventWindow[eventCenter-1,'speed'][[1]] - min(eventWindow[seq(eventCenter+1,eventCenter+15),'speed'])
    max_precip <- max(eventWindow$precip)
    max_precip.tstamp <- eventWindow[which(eventWindow$precip == max_precip), 'tstamp'][[1]]
    
    max_precip.prior <- max(eventWindow[seq(1,eventCenter-1),'precip'])
    max_precip.post <- max(eventWindow[seq(eventCenter+1,eventCenter+15),'precip'])
    precip.event <- eventWindow[eventCenter,'precip'][[1]] # precip during event
    avgPrecip.prior <- mean(eventWindow$precip[seq(1,eventCenter-1)])
    
    precipCnt.after <- sum(eventWindow[seq(eventCenter,eventCenter+15),'precip']>0)
    
    # Time to Recover (t2r)
    event.tstamp <- eventWindow[eventCenter,'tstamp'][[1]]
    recovery.speed <- eventWindow[eventCenter, 'avgSpd'][[1]] # this is the threshold for recovery
    # perhaps the recovery theshold should use the time step before spd.
    
    tmp3 <- eventWindow[as.integer(rownames(eventWindow))>=eventCenter,]
    end.tstamp <- tmp3[tmp3$speed >= recovery.speed,]$tstamp[1] # time of recovery (if not possible, produces NA)
    
    t2r <- difftime(end.tstamp,event.tstamp) %>% as.integer()
    #is.na(t2r)
    
    # Time to Impact (t2i)
    start.precip <- eventWindow[eventWindow$precip>0,]$tstamp[1]
    t2i <- difftime(event.tstamp, start.precip) %>% as.integer()
    
    # record all stats in a single data frame
    tmp.stats <- data.frame(segment=segment,
                            eventRow=i,
                            ds=ds,
                            max_precip=max_precip,
                            max_precip.tstamp=max_precip.tstamp,
                            max_precip.prior=max_precip.prior,
                            max_precip.post=max_precip.post,
                            precip.event=precip.event,
                            avgPrecip.prior=avgPrecip.prior,
                            precipCnt.after=precipCnt.after,
                            event.start=event.tstamp,
                            recovery.speed=recovery.speed,
                            recovery.tstamp=end.tstamp,
                            t2r=t2r,
                            precipStart.tstamp=start.precip,
                            t2i=t2i,
                            construction=eventWindow[eventCenter,'construction'][[1]])
    # following vars could be added to the output but seem to cause warnings that need investigated
    # hours=eventWindow[eventCenter,'hours'][[1]],
    # daylight=eventWindow[eventCenter,'daylight'][[1]],
    # dayofweek=eventWindow[eventCenter,'dayofweek'][[1]]
    
    segment.stats <- bind_rows(segment.stats, tmp.stats)
    
    
    # setting a break to check stats of first calculated event
    # if(precipPresent) {
    #   # DIAGNOSTIC PLOT
    #   # xyplot(speed+avgSpd+precip~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)
    #   break
    # }
    prevEvent <- i
  }

  return(segment.stats)
}

# subset
# tmp <- traffic %>% ungroup() %>%  filter(position>=102 & position<=123) %>% 
#   mutate(avgSpd=frollmean(x=speed,n=2,fill=NA))

# full set
tmp <- traffic %>% mutate(avgSpd=frollmean(x=speed,n=2,fill=NA))
# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 10, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)

uniqueSegs <- tmp %>% select(position) %>% unique()
segment.stats <- data.frame()
for (i in uniqueSegs[[1]]) {
  tmp2 <- tmp %>% filter(position==i)
  segment.stats <- bind_rows(segment.stats, generate_stats(i, segData=tmp2))
}


segStats.tmp <- segment.stats %>% filter(max_precip >= 0)
segStats.tmp$gradient <- segStats.tmp$precip.event - segStats.tmp$max_precip.prior

# positive difference would indicate that the precip was stronger than prior to when the event occurred
xyplot(gradient~ds | factor(construction), data=segStats.tmp, pch=16, alpha=0.2,
       layout=c(1,2,1))
smoothScatter(segStats.tmp$ds, segStats.tmp$gradient)

xyplot(max_precip.prior~ds | factor(construction), data=segStats.tmp, pch=16, alpha=0.2,
       layout=c(1,2,1))
smoothScatter(segStats.tmp$ds, segStats.tmp$max_precip.prior)

# may be a good one, shows that the process is slightly bimodal.
densityplot(~ds, data=segStats.tmp, auto.key=T, groups=construction, plot.points=F,
            axis=axis.grid, xlim=c(-50,80),
            scales=list(x=list(at=seq(-50,80,10)))
            )

# define precipitation prior to the event into categories for boxplot purposes
precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
#c(0,0.01,2.5,5,10,20,40,60,80,100,140) #old ranges
segStats.tmp$rcat <- cut(segStats.tmp$max_precip.prior,
                         breaks=precip.ranges, right=F, include.lowest=T)

# Plan here was to label the sample size on the plot, ggplot may be better for this.
# bwplot(rcat~ds, data=segStats.tmp,
#        panel=function(x, y, ...){
#          panel.bwplot(x, y, ...)
#          panel.text(10, 5, labels='Hey')
#          panel.text(12, 4.5, labels='Yo!')
#        })

segStats.tmp$construction <- as.factor(segStats.tmp$construction) 
levels(segStats.tmp$construction) <- c('Non-Construction','Construction')
bwplot(rcat~ds | factor(construction), data=segStats.tmp, do.out=F,
       xlab='Delta Speed (mph)', ylab='Precipitation Range (mm/hr)', 
       axis=axis.grid, layout=c(1,2,1), xlim=c(-20,60),
       scales=list(x=list(at=seq(-20,60,10)))
       )

# percentage of negative ds events
length(segStats.tmp[segStats.tmp$ds < 0, 'ds'])/length(segStats.tmp$ds) * 100


bwplot(rcat~t2r | factor(construction), data=segStats.tmp, do.out=F, 
       xlab='Time to Recovery (mins)', ylab='Precipitation Range (mm/hr)',
       axis=axis.grid, xlim=c(0,30.2), 
       scales=list(x=list(at=seq(0,30,5))),
       layout=c(1,2,1))

# bwplot(factor(precipCnt.after)~t2r | factor(construction), data=segStats.tmp, layout=c(1,2,1))
# percentage of non-recovery events
sum(is.na(segStats.tmp$t2r))/length(segStats.tmp$t2r) * 100


# t2i doesn't work very well as is.
bwplot(rcat~t2i | factor(construction), data=segStats.tmp, do.out=F, 
       xlab='Time to Impact (mins)', ylab='Precipitation Range (mm/hr)',
       axis=axis.grid, xlim=c(0,30.2), 
       scales=list(x=list(at=seq(0,30,5))),
       layout=c(1,2,1))

# filter 30 minute impacts since so many are 30 minutes
segStats.tmp2 <- segStats.tmp %>% filter(t2i<30)
bwplot(rcat~t2i | factor(construction), data=segStats.tmp2, do.out=F, 
       xlab='Time to Impact (mins)', ylab='Precipitation Range (mm/hr)',
       axis=axis.grid, xlim=c(-0.5,30), 
       scales=list(x=list(at=seq(0,30,5))),
       layout=c(1,2,1))

# percentage of 30 minute impact events
length(segStats.tmp2$t2i)/length(segStats.tmp$t2i) * 100



# tallying up the sample sizes
segStats.summary <- segStats.tmp %>% group_by(rcat,construction) %>% tally()
max(segStats.tmp$ds)
min(segStats.tmp$ds)



ds.summary <- segStats.tmp %>% group_by(construction,rcat) %>% 
  summarise(min=min(ds), max=max(ds), median=median(ds), std=sd(ds), mean=mean(ds), n=n())

t2i.summary <- segStats.tmp %>% group_by(construction,rcat) %>% 
  summarise(min=min(t2i), max=max(t2i), median=median(t2i), std=sd(t2i), mean=mean(t2i), n=n())

t2i.summary.no30 <- segStats.tmp2 %>% group_by(construction,rcat) %>% 
  summarise(min=min(t2i), max=max(t2i), median=median(t2i), std=sd(t2i), mean=mean(t2i), n=n())

t2r.summary <- segStats.tmp %>% group_by(construction,rcat) %>% 
  summarise(min=min(t2r), max=max(t2r), median=median(t2r), std=sd(t2r), mean=mean(t2r), n=n())


mean(segStats.tmp$t2i)
mean(segStats.tmp$t2r, na.rm=T)
