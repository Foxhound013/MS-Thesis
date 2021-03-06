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


generate_stats <- function(segment, segData, plot_it=F) {
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
    
    if (plot_it) {
      png(paste0('./figures/automatedEventFigs/',segment,'_',i,'.png'))
      p <- xyplot(speed+avgSpd+precip~tstamp,data=eventWindow, type=c('p','l','g'), auto.key=T)
      print(p)
      dev.off()
    }
    
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
length(segment.stats[segment.stats$ds < 0, 'ds'])/length(segment.stats$ds) * 100
# 1.7% marked as false events

# percentage of score 20
length(segment.stats[segment.stats$score == 20, 'score'])/length(segment.stats$score) * 100
# 10% of events were score of 20

# filter events
segStats.tmp <- segment.stats %>% filter(ds >= 0) # removes negative DS (or false event trips)
segStats.tmp <- segStats.tmp %>% filter(score == 30)

rm.crashes <- function(segstats) {
  # drop out known crashes
  c1 <- do(segstats, data.frame(x = which((.$event.start=='2018-06-08 20:38:00' |
                                            .$event.start == '2018-06-08 20:40:00' |
                                            .$event.start == '2018-06-08 20:42:00' |
                                            .$event.start == '2018-06-08 20:44:00' |
                                            .$event.start == '2018-06-08 20:46:00' |
                                            .$event.start == '2018-06-08 20:48:00' |
                                            .$event.start == '2018-06-08 20:50:00' |
                                            .$event.start == '2018-06-08 20:52:00' |
                                            .$event.start == '2018-06-08 20:54:00' |
                                            .$event.start == '2018-06-08 20:56:00' |
                                            .$event.start == '2018-06-08 20:58:00' |
                                            .$event.start == '2018-06-08 21:00:00' |
                                            .$event.start == '2018-06-08 21:02:00' |
                                            .$event.start == '2018-06-08 21:04:00' |
                                            .$event.start == '2018-06-08 21:06:00' |
                                            .$event.start == '2018-06-08 21:08:00') & 
                                           (.$startmm>=100 & .$startmm<=110)
  )
  )
  ) %>% .$x
  
  c2 <- do(segstats, data.frame(x = which((.$event.start=='2018-06-10 19:30:00' |
                                            .$event.start == '2018-06-10 19:32:00' |
                                            .$event.start == '2018-06-10 19:34:00' |
                                            .$event.start == '2018-06-10 19:36:00' |
                                            .$event.start == '2018-06-10 19:38:00' |
                                            .$event.start == '2018-06-10 19:40:00' |
                                            .$event.start == '2018-06-10 19:42:00' |
                                            .$event.start == '2018-06-10 19:44:00' |
                                            .$event.start == '2018-06-10 19:46:00' |
                                            .$event.start == '2018-06-10 19:48:00' |
                                            .$event.start == '2018-06-10 19:50:00' |
                                            .$event.start == '2018-06-10 19:52:00' |
                                            .$event.start == '2018-06-10 19:54:00' |
                                            .$event.start == '2018-06-10 19:56:00' |
                                            .$event.start == '2018-06-10 19:58:00' |
                                            .$event.start == '2018-06-10 20:00:00') & 
                                           (.$startmm>=56 & .$startmm<=66)
  )
  )
  ) %>% .$x
  
  c3 <- do(segstats, data.frame(x = which((.$event.start=='2018-06-12 06:00:00' |
                                            .$event.start == '2018-06-12 06:02:00' |
                                            .$event.start == '2018-06-12 06:04:00' |
                                            .$event.start == '2018-06-12 06:06:00' |
                                            .$event.start == '2018-06-12 06:08:00' |
                                            .$event.start == '2018-06-12 06:10:00' |
                                            .$event.start == '2018-06-12 06:12:00' |
                                            .$event.start == '2018-06-12 06:14:00' |
                                            .$event.start == '2018-06-12 06:16:00' |
                                            .$event.start == '2018-06-12 06:18:00' |
                                            .$event.start == '2018-06-12 06:20:00' |
                                            .$event.start == '2018-06-12 06:22:00' |
                                            .$event.start == '2018-06-12 06:24:00' |
                                            .$event.start == '2018-06-12 06:26:00' |
                                            .$event.start == '2018-06-12 06:28:00' |
                                            .$event.start == '2018-06-12 06:30:00') & 
                                           (.$startmm>=62 & .$startmm<=72)
  )
  )
  ) %>% .$x
  
  c4 <- do(segstats, data.frame(x = which((.$event.start=='2018-06-21 18:50:00' |
                                            .$event.start == '2018-06-21 18:52:00' |
                                            .$event.start == '2018-06-21 18:54:00' |
                                            .$event.start == '2018-06-21 18:56:00' |
                                            .$event.start == '2018-06-21 18:58:00' |
                                            .$event.start == '2018-06-21 19:00:00' |
                                            .$event.start == '2018-06-21 19:02:00' |
                                            .$event.start == '2018-06-21 19:04:00' |
                                            .$event.start == '2018-06-21 19:06:00' |
                                            .$event.start == '2018-06-21 19:08:00' |
                                            .$event.start == '2018-06-21 19:10:00' |
                                            .$event.start == '2018-06-21 19:12:00' |
                                            .$event.start == '2018-06-21 19:14:00' |
                                            .$event.start == '2018-06-21 19:16:00' |
                                            .$event.start == '2018-06-21 19:18:00' |
                                            .$event.start == '2018-06-21 19:20:00') & 
                                           (.$startmm>=128 & .$startmm<=138)
  )
  )
  ) %>% .$x
  
  c5 <- do(segstats, data.frame(x = which((.$event.start=='2018-06-21 17:30:00' |
                                            .$event.start == '2018-06-21 17:32:00' |
                                            .$event.start == '2018-06-21 17:34:00' |
                                            .$event.start == '2018-06-21 17:36:00' |
                                            .$event.start == '2018-06-21 17:38:00' |
                                            .$event.start == '2018-06-21 17:40:00' |
                                            .$event.start == '2018-06-21 17:42:00' |
                                            .$event.start == '2018-06-21 17:44:00' |
                                            .$event.start == '2018-06-21 17:46:00' |
                                            .$event.start == '2018-06-21 17:48:00' |
                                            .$event.start == '2018-06-21 17:50:00' |
                                            .$event.start == '2018-06-21 17:52:00' |
                                            .$event.start == '2018-06-21 17:54:00' |
                                            .$event.start == '2018-06-21 17:56:00' |
                                            .$event.start == '2018-06-21 17:58:00' |
                                            .$event.start == '2018-06-21 18:00:00') & 
                                           (.$startmm>=172 & .$startmm<=182)
  )
  )
  ) %>% .$x
  
  c6 <- do(segstats, data.frame(x = which((.$event.start=='2018-06-26 14:50:00' |
                                            .$event.start == '2018-06-26 14:52:00' |
                                            .$event.start == '2018-06-26 14:54:00' |
                                            .$event.start == '2018-06-26 14:56:00' |
                                            .$event.start == '2018-06-26 14:58:00' |
                                            .$event.start == '2018-06-26 15:00:00' |
                                            .$event.start == '2018-06-26 15:02:00' |
                                            .$event.start == '2018-06-26 15:04:00' |
                                            .$event.start == '2018-06-26 15:06:00' |
                                            .$event.start == '2018-06-26 15:08:00' |
                                            .$event.start == '2018-06-26 15:10:00' |
                                            .$event.start == '2018-06-26 15:12:00' |
                                            .$event.start == '2018-06-26 15:14:00' |
                                            .$event.start == '2018-06-26 15:16:00' |
                                            .$event.start == '2018-06-26 15:18:00' |
                                            .$event.start == '2018-06-26 15:20:00') & 
                                           (.$startmm>=110 & .$startmm<=120)
  )
  )
  ) %>% .$x
  
  all_crashes <- c(c1,c2,c3,c4,c5,c6) %>% unique()
  
  segstats <- segstats[-all_crashes,]
  return(segstats)
}

segStats.tmp <- rm.crashes(segStats.tmp)

sum(segStats.tmp$precipPresent)/nrow(segStats.tmp) * 100 
# 17.6 % of events are precipitable

sum(segStats.tmp$construction)/nrow(segStats.tmp) * 100
# 33% are in the presence of construction


# define precipitation prior to the event into categories for boxplot purposes
precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
#precip.ranges <- c(0,0.01,2.5,5,10,150)
#c(0,0.01,2.5,5,10,20,40,60,80,100,140) #old ranges
segStats.tmp$rcat <- cut(segStats.tmp$max_precip.prior,
                         breaks=precip.ranges, right=F, include.lowest=T)

# convert other categories to their proper factors and levels
segStats.tmp <- segStats.tmp %>% mutate(hour.range=ifelse(segStats.tmp$hours >= 4 & segStats.tmp$hours < 10,
                                                yes='Morning', 
                                                no=ifelse(segStats.tmp$hours >= 10 & segStats.tmp$hours < 16,
                                                          yes='Morning Rush',
                                                          no=ifelse(segStats.tmp$hours >= 16 & segStats.tmp$hours < 22,
                                                                    yes='Afternoon Rush',
                                                                    no='Evening')
                                                )
)
)
segStats.tmp$hour.range <- as.factor(segStats.tmp$hour.range)
levels(segStats.tmp$hour.range) <- levels(segStats.tmp$hour.range)[c(3,1,2)]

hour.arrangement <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14',
                      '15','16','17','18','19','20','21','22','23')
segStats.tmp$hours <- factor(segStats.tmp$hours, levels=hour.arrangement, ordered=T)

segStats.tmp$dayofweek <- factor(segStats.tmp$dayofweek, 
                                 levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                                 ordered=T)

segStats.tmp$weekend <- ifelse(segStats.tmp$dayofweek== 'Saturday' | segStats.tmp$dayofweek=='Sunday',
                               yes='Weekend', no='Weekday')
segStats.tmp$weekend <- factor(segStats.tmp$weekend,
                               levels=c('Weekday','Weekend'),
                               ordered=T)

segStats.tmp$construction <- ifelse(segStats.tmp$construction, yes='Construction', no='Non-Construction')
segStats.tmp$construction <- factor(segStats.tmp$construction,
                                    levels=c('Non-Construction','Construction'),
                                    ordered=T) 

segStats.tmp$precipPresent <- ifelse(segStats.tmp$precipPresent, yes='Precipitable', no='Non-Precipitable')
segStats.tmp$precipPresent <- factor(segStats.tmp$precipPresent,
                                     levels=c('Precipitable', 'Non-Precipitable'),
                                     ordered=T)





# Delta Speed Plots

# event sparsity is too much for the plot to be super useful
png('./figures/event_bwplots/eventsVds%02d.png', units='in', res=220, width=8.5, height=6)
bwplot(rcat~ds | weekend*hour.range*construction, 
       data=segStats.tmp, axis=axis.grid, do.out=T, as.table=T, 
       layout=c(6,2), cex=.3,
       par.settings = list(plot.symbol = list(pch = 16, cex=0.3, alpha=0.5)),
       par.strip.text=list(cex=.68),
       xlab='Delta Speed (mph)',
       ylab='Precipitation Rate (mm/hr)',
       scales=list(x=list(at=seq(0,80,10), cex=.7, rot=90),
                   y=list(cex=0.7))
)
dev.off()

# likely a good plot from process diagnostic standpoint.
png('./figures/event_ecdf/ecdf_ds%02d.png', units='in', res=220, width=8.5, height=6)
ecdfplot(~ds | weekend*hour.range*construction, data=segStats.tmp,
         groups=precipPresent, as.table=T,
         par.strip.text=list(cex=.68),
         axis=axis.grid, from=0, to=80,
         xlab='Delta Speed (mph)',
         layout=c(6,2), auto.key=list(cex=0.73),
         scale=list(x=list(at=seq(0,80,10), cex=0.7, rot=90),
                    y=list(cex=0.7))
         )
dev.off()


# good plot, hours can be used here as this still provides a good representation overall
stripplot(ds~rcat | weekend*hours, data=segStats.tmp, groups=construction,
          layout=c(1,4), jitter.data=T, pch=16, alpha=0.3,
          axis=axis.grid, auto.key=T,
          scales=list(x=list(rot=90)))


# Good plot
pdf('./figures/densityPlot_dsFaceted.pdf')
densityplot(~ds | weekend*hour.range*construction, data=segStats.tmp, auto.key=T, groups=precipPresent, 
            axis=axis.grid, layout=c(6,2), xlim=c(0,80), plot.points=F,
            scales=list(x=list(at=seq(0,80,10), cex=0.8, rot=90)),
            from=0, to=80)
dev.off()


# Time to Recovery Plots

# what is the percentage of non-recoveries?
sum(is.na(segStats.tmp$t2r))/nrow(segStats.tmp) * 100
# 36.2% of events detected are non-recovery

png('./figures/event_bwplots/bwplot_t2r%02d.png', units='in', res=220, width=8.5, height=6)
bwplot(rcat~t2r | factor(weekend)*factor(hour.range)*factor(construction), 
       data=segStats.tmp, axis=axis.grid, do.out=T, as.table=T, 
       layout=c(6,2), cex=0.3,
       par.settings = list(plot.symbol = list(pch = 16, cex=0.3, alpha=0.5)),
       par.strip.text=list(cex=.68),
       xlab='Time to Recovery (minutes)',
       ylab='Precipitation Rate (mm/hr)',
       scales=list(x=list(at=seq(0,30,5), cex=.7, rot=90),
                   y=list(cex=0.7))
)
dev.off()



# likely a good plot from process diagnostic standpoint.
png('./figures/event_ecdf/ecdf_t2r%02d.png', units='in', res=220, width=8.5, height=6)
ecdfplot(~t2r | weekend*hour.range*construction, data=segStats.tmp,
         groups=precipPresent, as.table=T,
         par.strip.text=list(cex=.68),
         auto.key=list(cex=0.73),
         xlab='Time to Recovery (minutes)',
         axis=axis.grid, from=0, to=30,
         layout=c(6,2),
         scale=list(x=list(cex=0.7),
                    y=list(cex=0.7))
         )
dev.off()


# Time to Impact plots


png('./figures/event_bwplots/bwplot_t2i.png', units='in', res=220, width=8.5, height=6)
bwplot(rcat~t2i | weekend*hour.range*construction, 
       data=segStats.tmp, axis=axis.grid, do.out=T, as.table=T, 
       layout=c(6,2), cex=0.3,
       par.settings = list(plot.symbol = list(pch = 16, cex=0.3, alpha=0.5)),
       par.strip.text=list(cex=.68),
       xlab='Time to Impact (minutes)',
       ylab='Precipitation Rate (mm/hr)',
       scales=list(x=list(at=seq(0,30,5), cex=.7, rot=90),
                   y=list(cex=0.7))
)
dev.off()








# filter 30 minute impacts since so many are 30 minutes
segStats.tmp2 <- segStats.tmp %>% filter(t2i<30)

# percentage of 30 minute impact events
length(segStats.tmp2$t2i)/length(segStats.tmp$t2i) * 100
# only 6.7% of the events are less than a 30 minute t2i



# Now that figures have been compiled need to collect sample size information in tabular format




# tallying up the sample sizes
segStats.summary <- segStats.tmp %>% group_by(rcat,weekend,hour.range,construction) %>% tally()
max(segStats.tmp$ds)
min(segStats.tmp$ds)

png('./figures/sampleSizes/event_samples_all.png', units='in', res=220, width=8.5, height=6)
dotplot(rcat~n | weekend*hour.range*construction, 
        data=segStats.summary,
        as.table=T,
        axis=axis.grid,
        pch=16, cex=0.5,
        par.strip.text=list(cex=.68),
        main='Sample Sizes for Precipitation Regimes and Conditions',
        xlab='Precipitation Rate (mm/hr)',
        ylab='Sample Size',
        layout=c(6,2),
        scales=list(x=list(cex=.7),
                    y=list(cex=0.7))
        )
dev.off()

segStats.summary.fewer <- segStats.summary %>% filter(n <= 100)

png('./figures/sampleSizes/event_samples_lessorequal100.png', units='in', res=220, width=8.5, height=6)
dotplot(rcat~n | weekend*hour.range*construction, 
        data=segStats.summary.fewer,
        as.table=T,
        axis=axis.grid,
        pch=16, cex=0.5,
        par.strip.text=list(cex=.68),
        main='Sample Sizes for Precipitation Regimes and Conditions (<= 100)',
        xlab='Precipitation Rate (mm/hr)',
        ylab='Sample Size',
        layout=c(6,2),
        xlim=c(0,100),
        scales=list(x=list(at=seq(0,100,20),cex=.7),
                    y=list(cex=0.7))
)
dev.off()



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





