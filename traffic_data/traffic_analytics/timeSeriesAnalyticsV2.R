# This updated implemntation of the original time series statistics generation
# takes into account precipitable and non-precitable events, generating the
# appropriate statistics for each set of data. A secondary algorithm is implemented
# here to calculate some baseline statistics about standard traffic flow

# This re-imagining of the time series analytics should provide a robust way of describing
# free flow conditions as well as event conditions, both precipitable and non-precipitable.

# From these characterizations of each of these states, a percent reduction from the standard
# can be calculated.


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


generate_stats <- function(segment, segData) {
  # window building 
  eventRow <- which(segData$event == T)
  
  segment.stats <- data.frame()
  eventCenter <- 16
  prevEvent <- 0
  
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
    precipPresent <- ifelse(sum(eventWindow[seq(1,eventCenter),'precip']>0) > 0, yes=T, no=F)
    
    # png(paste0('./figures/automatedEventFigs/',segment,'_',i,'.png'))
    # p <- xyplot(speed+avgSpd+precip~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)
    # print(p)
    # dev.off()
    
    # generate statistics
    # Delta Speed (ds)
    min.spd.post <- min(eventWindow[seq(eventCenter+1,eventCenter+15),'speed'])
    spd.prior <- eventWindow[eventCenter-1,'speed'][[1]]
    ds <-  spd.prior - min.spd.post
    
    event.tstamp <- eventWindow[eventCenter,'tstamp'][[1]]
    
    # Time to Recover (t2r)
    
    recovery.speed <- eventWindow[eventCenter, 'avgSpd'][[1]] # this is the threshold for recovery
    # perhaps the recovery theshold should use the time step before spd.
    
    tmp3 <- eventWindow[as.integer(rownames(eventWindow))>=eventCenter,]
    end.tstamp <- tmp3[tmp3$speed >= recovery.speed,]$tstamp[1] # time of recovery (if not possible, produces NA)
    
    t2r <- difftime(end.tstamp,event.tstamp) %>% as.integer()
    #is.na(t2r)
    
    # precip stats
    max_precip <- max(eventWindow$precip)
    max_precip.prior <- max(eventWindow[seq(1,eventCenter-1),'precip'])
    max_precip.post <- max(eventWindow[seq(eventCenter+1,eventCenter+15),'precip'])
    precip.event <- eventWindow[eventCenter,'precip'][[1]] # precip during event
    avgPrecip.prior <- mean(eventWindow$precip[seq(1,eventCenter-1)])
    
    # precipitation specific statistics
    if (precipPresent == T) {
      # precip stats
      max_precip.tstamp <- eventWindow[which(eventWindow$precip == max_precip), 'tstamp'][[1]]
      precipCnt.after <- sum(eventWindow[seq(eventCenter,eventCenter+15),'precip']>0)
      
      # Time to Impact (t2i)
      start.precip <- eventWindow[eventWindow$precip>0,]$tstamp[1]
      t2i <- difftime(event.tstamp, start.precip) %>% as.integer()
    } else {
      # max_precip <- NA
      max_precip.tstamp <- NA
      # max_precip.prior <- NA
      # max_precip.post <- NA
      # precip.event <- NA
      # avgPrecip.prior <- NA
      precipCnt.after <- NA
      start.precip <- NA
      t2i <- NA
    }
    
    # browser()
    
    # record all stats in a single data frame
    tmp.stats <- data.frame(segment=segment,
                            startmm=eventWindow[eventCenter,'startmm'][[1]],
                            eventRow=i,
                            precipPresent=precipPresent,
                            ds=ds,
                            spd.prior=spd.prior,
                            min.spd.post=min.spd.post,
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
                            construction=eventWindow[eventCenter,'construction'][[1]],
                            hours=as.character(eventWindow[eventCenter,'hours'][[1]]),
                            daylight=as.character(eventWindow[eventCenter,'daylight'][[1]]),
                            dayofweek=as.character(eventWindow[eventCenter,'dayofweek'][[1]]),
                            score=eventWindow[eventCenter,'score'][[1]]
    )
    # following vars could be added to the output but seem to cause warnings that need investigated
    
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

# subset for testing purposes
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


# percentage of negative ds events
length(segStats.tmp[segStats.tmp$ds < 0, 'ds'])/length(segStats.tmp$ds) * 100

segStats.tmp <- segment.stats %>% filter(ds >= 0) # removes negative DS (or false event trips)
segStats.tmp <- segStats.tmp %>% filter(score == 30)

sum(segStats.tmp$precipPresent)/nrow(segStats.tmp) * 100

sum(segStats.tmp$construction)/nrow(segStats.tmp) * 100


# define precipitation prior to the event into categories for boxplot purposes
precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
#precip.ranges <- c(0,0.01,2.5,5,10,150)
#c(0,0.01,2.5,5,10,20,40,60,80,100,140) #old ranges
segStats.tmp$rcat <- cut(segStats.tmp$max_precip.prior,
                         breaks=precip.ranges, right=F, include.lowest=T)

# convert other categories to their proper factors and levels
segStats.tmp$hours <- as.factor(segStats.tmp$hours)
levels(segStats.tmp$hours) <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14',
                                '15','16','17','18','19','20','21','22','23')
segStats.tmp$dayofweek <- as.factor(segStats.tmp$dayofweek)
levels(segStats.tmp$dayofweek) <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')

segStats.tmp$weekend <- ifelse(segStats.tmp$dayofweek== 'Saturday' | segStats.tmp$dayofweek=='Sunday',
                               yes='Weekend', no='Weekday')
segStats.tmp$weekend <- as.factor(segStats.tmp$weekend)

segStats.tmp$construction <- as.factor(segStats.tmp$construction) 
levels(segStats.tmp$construction) <- c('Non-Construction','Construction')

segStats.tmp$precipPresent <- as.factor(segStats.tmp$precipPresent)
levels(segStats.tmp$precipPresent) <- c('Non-Precipitable', 'Precipitable')


# event sparsity is too much for the plot to be super useful
pdf('./figures/eventsVds.pdf', width=10, height=7)
bwplot(factor(precipPresent)~ds | factor(dayofweek)*factor(hours)*factor(construction), 
       data=segStats.tmp, layout=c(7,3),
       axis=axis.grid, do.out=F,
       scales=list(x=list(at=seq(0,80,10), cex=.8, rot=90))
)
dev.off()


# good plot
stripplot(ds~rcat | weekend*hours, data=segStats.tmp, groups=construction,
          layout=c(1,4), jitter.data=T, pch=16, alpha=0.3,
          axis=axis.grid, auto.key=T,
          scales=list(x=list(rot=90)))


# Good plot
pdf('./figures/densityPlot_dsFaceted.pdf')
densityplot(~ds | weekend*hours*construction, data=segStats.tmp, auto.key=T, groups=precipPresent, 
            axis=axis.grid, layout=c(6,2), xlim=c(0,80), plot.points=F,
            scales=list(x=list(at=seq(0,80,10), cex=0.8, rot=90)),
            from=0, to=80)
dev.off()


ecdfplot(~ds | weekend*hours*construction, data=segStats.tmp, groups=precipPresent,
         axis=axis.grid, layout=c(6,2))



# GOOD PLOT
test <- segStats.tmp %>% group_by(weekend,hours,precipPresent,rcat,construction) %>% 
  summarise(ds=median(ds))

pdf('./figures/levPlot.pdf')
levelplot(ds~factor(hours)*rcat | weekend*precipPresent*construction, 
          data=test, col.regions=rev(heat.colors(30)), alpha=0.2,
          xlab='Hour of Day (UTC)', ylab='Precipitation Rate (mm/hr)',
          main='Median DS by Hour of Day')
dev.off()



segStats.tmp$dsCat <- cut(segStats.tmp$ds, breaks=c(0,5,10,15,20,30,40,80), 
                          right=F, include.lowest=T)
ds_rain <- segStats.tmp %>% group_by(rcat,dsCat) %>% tally()

pdf('./figures/segmentDS.pdf')
xyplot(segment~ds | dayofweek*hours, data=segStats.tmp, groups=precipPresent,
       layout=c(7,2), axis=axis.grid, pch=16, alpha=0.3, cex=0.5)
dev.off()

densityplot(~ds | hours, data=segStats.tmp, groups=precipPresent,
            layout=c(7,4,1), plot.points=F, auto.key=T)



# need to encode the sample size
bwplot(rcat~ds | weekend*construction*hours, data=segStats.tmp, do.out=F,
       xlab='Delta Speed (mph)', ylab='Precipitation Range (mm/hr)', 
       axis=axis.grid, layout=c(1,2), xlim=c(-20,60), 
       scales=list(x=list(at=seq(-20,60,10)))
)



stripplot(t2r~rcat | weekend*hours, data=segStats.tmp, groups=construction,
          layout=c(1,4), jitter.data=T, pch=16, alpha=0.3,
          axis=axis.grid, auto.key=T,
          scales=list(x=list(rot=90)))

stripplot(t2i~rcat | weekend*hours, data=segStats.tmp, groups=construction,
          layout=c(1,4), jitter.data=T, pch=16, alpha=0.3,
          axis=axis.grid, auto.key=T,
          scales=list(x=list(rot=90)))




# percentage of non-recovery events
sum(is.na(segStats.tmp$t2r))/length(segStats.tmp$t2r) * 100




# filter 30 minute impacts since so many are 30 minutes
segStats.tmp2 <- segStats.tmp %>% filter(t2i<30)

stripplot(t2i~rcat | weekend*hours, data=segStats.tmp2, groups=construction,
          layout=c(1,4), jitter.data=T, pch=16, alpha=0.3,
          axis=axis.grid, auto.key=T,
          scales=list(x=list(rot=90)))

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





# locates event windows and removes the corresponding data in order to produce information
# about the baseline traffic (i.e. non events).
generate_baseline <- function(segment, segData) {
  #REPLACE THE TMP VAR WITH SEGDATA
  eventRows <-  which(segData$event == T)
  removalRows <- vector()
  
  for (i in eventRows) {
    removalRows <-  append(removalRows, seq(i-15,i+15))
  }
  removalRows <- removalRows %>% unique()
  
  tmp <- tmp[-c(removalRows),] # drop out all event associated rows
  
  # calculate stats per
  segBaseline.stats <- data.frame(segment=segment,
                                  startmm=segData$startmm[[1]],
                                  median.spd=median(segData$speed),
                                  avg.spd=mean(segData$speed)
  )
  
  return(segBaseline.stats)
  
}

# full set
tmp <- traffic %>% mutate(avgSpd=frollmean(x=speed,n=2,fill=NA))
# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 10, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)

#REPLACE THE TMP VAR WITH SEGDATA
eventRows <-  which(tmp$event == T)
removalRows <- vector()

for (i in eventRows) {
  removalRows <-  append(removalRows, seq(i-15,i+15))
}
removalRows <- removalRows %>% unique()

tmp <- tmp[-c(removalRows),] # drop out all event associated rows
tmp$weekend <- ifelse(tmp$dayofweek== 'Saturday' | tmp$dayofweek=='Sunday',
                      yes='Weekend', no='Weekday')
tmp$weekend <- as.factor(tmp$weekend)

segBaseline <- tmp %>% filter(score == 30)
# segBaseline <- tmp %>% filter(score == 30) %>% group_by(startmm, hours, construction, weekend) %>% 
#   summarise(median.speed=median(speed),
#             mean.speed=round(mean(speed),2)
#             )
# segBaseline$diff <- segBaseline$median.speed - segBaseline$mean.speed


# the seg baseline describes speeds in which traffic events were not tripped.
# need to describe these in full.
