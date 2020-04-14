library(data.table)
library(sf)
library(tmap)
library(mapview)
library(dplyr)
library(tidyr)
library(lattice)

fpath <- 'C:/Users/Downi/Desktop/June_I-65_2018.csv'
traffic <- fread(fpath,col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                                   'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
glimpse(traffic)

traffic <- traffic %>% filter(direction=='N')
uniqueSegs <- traffic[,c('xdid','position','lon','lat')] %>% unique %>% st_as_sf(coords=c('lon','lat'))

tmap_mode('view')
tm_shape(uniqueSegs) + tm_dots() + tm_scale_bar()

# fix up the data
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='UTC')
#traffic <- complete(traffic, xdid, tstamp) # not necessary
# before complete: 39,830,400
# after complete: 39,830,400
traffic <- setDT(traffic)

# segment t series by confidence level
hConf <- traffic[which(score==30),]
mConf <- traffic[which(score==20),]
lConf <- traffic[which(score==10),]

length(hConf$xdid)/length(traffic$xdid) * 100
length(mConf$xdid)/length(traffic$xdid) * 100
length(lConf$xdid)/length(traffic$xdid) * 100

# isolate the time series data for completing the subsets
tSeries <- traffic[,c('xdid','tstamp')]


# day of week and time of day distribution
weekdaySeq <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
hourSeq <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14',
             '15', '16', '17', '18', '19', '20', '21', '22', '23')
traffic$weekday <- weekdays(traffic$tstamp)
traffic$weekday <- factor(traffic$weekday, levels=weekdaySeq)
traffic$hours <- hour(traffic$tstamp)
traffic$hours <- factor(traffic$hours, levels=hourSeq)

weekdayDistro <- traffic %>% group_by(weekday, score) %>% tally() %>% 
        mutate(percent=(n/length(traffic$xdid))*100)
weekdayDistro$score <- ifelse(weekdayDistro$score==20, yes='Medium-confidence', no='High-confidence')
weekdayDistro$score <- factor(weekdayDistro$score)

# DO NOT USE THIS PLOT
# suffers from sample size of the data poitns available
png('./figures/ConfidenceValuesByPercentage_AndDayOfWeek.png', width=3, heigh=3, res=300, units='in')
xyplot(percent~weekday, data=weekdayDistro, groups=score, 
       cex=0.9,
       main=list(label='Confidence Values by Percentage and Day of Week',
               cex=0.7),
       ylab=list(label='Percent of Dataset', cex=0.5),
       xlab=list(label='Day of Week', cex=0.5),
       auto.key=list(columns=2, space='top', cex=0.4), 
       type=c('p','l','g'), ylim=c(0,18), 
       scales=list(x=list(rot=45),
                   y=list(at=seq(0,20,2)),
                   cex=0.4)
       )
dev.off()

# verify that percentages add up to 1
sum(weekdayDistro$percent)

hourDistro <- traffic %>% group_by(hours, score) %>% tally() %>% 
        mutate(percent=(n/length(traffic$xdid))*100)
hourDistro$score <- ifelse(hourDistro$score==20, yes='Medium-confidence', no='High-confidence')
hourDistro$score <- factor(hourDistro$score)

xyplot(percent~hours, data=hourDistro, groups=score, 
       auto.key=T, type=c('p','l','g'))

png('./figures/ConfidenceValuesByPercentage_AndHourofDay.png', width=3, heigh=3, res=300, units='in')
xyplot(percent~hours, data=hourDistro, groups=score, 
       cex=0.4,
       pch=16,
       col=c('#0080FF','#FF40FF'),
       main=list(label='Confidence Values by Percentage and Hour of Day',
                 cex=0.7),
       ylab=list(label='Percent of Dataset', cex=0.5),
       xlab=list(label='Hour of Day (UTC)', cex=0.5),
       key=list(x=-0.08, y=1.1, corner=c(0,0), columns=2,
                points=list(pch=16,col=c('#0080FF','#FF40FF'),cex=0.4),
                text=list(levels(hourDistro$score), cex=0.5)
                ),
       type=c('p','l','g'), ylim=c(0,5), 
       scales=list(x=list(rot=90),
                   y=list(at=seq(0,5,.5)),
                   cex=0.4)
)
dev.off()



# verify that percentages add up to 1
sum(hourDistro$percent)

##### Low confidence segments #####
paste0(round((length(lConf$xdid)/length(traffic$xdid))*100, 3), '% of the data')

summary(lConf)
length(unique(lConf$xdid)) 
lConf <- complete(lConf, xdid, tstamp)

xyplot(speed~tstamp | factor(xdid), lConf, type='l', ylim=c(0,70),
       main='Speed Vs Time for Low Confidence Segment', ylab='Speed', xlab='Time Stamp')

# locate the segments spatially
lSeg <- lConf[,c('xdid','lon','lat')] %>% unique %>% st_as_sf(coords=c('lon','lat'))
tm_shape(lSeg) + tm_dots('xdid', style='cat')

# no point in seing the temporal spread, it's continuous from the start to end.


##### Medium confidence segments #####
paste0(round((length(mConf$xdid)/length(traffic$xdid))*100, 3), '% of the data')

summary(mConf)
length(unique(mConf$xdid))

#add in day and hour data
mConf$weekday <- weekdays(mConf$tstamp)
mConf$hour <- hour(mConf$tstamp)
# complete the data set
mConf.complete <- left_join(tSeries, mConf, by=c('xdid','tstamp'))


pdf('./figures/mConf_Tseries.pdf')
xyplot(speed~tstamp | factor(xdid), data=mConf.complete, type='l', pch=16, col='deepskyblue3',
       layout=c(1,6), ylim=seq(0,70,10))
dev.off()




mConf.weekday <- data.table(table(mConf.complete$xdid,mConf.complete$weekday))
colnames(mConf.weekday) <- c('xdid','weekday','frequency')
mConf.weekday$weekday <- factor(mConf.weekday$weekday, levels=weekdaySeq)

mConfWeekday.count <- data.table(mConf.weekday[,sum(frequency),by=c('weekday')])
colnames(mConfWeekday.count) <- c('weekday', 'frequency')
mConfWeekday.count$weekday <- factor(mConfWeekday.count$weekday, levels=weekdaySeq)
mConfWeekday.count$percent <- (mConfWeekday.count$frequency/length(traffic$tstamp))*100
sum(mConfWeekday.count$percent) # verifies validity of the data

unique_mConf <- mConf %>% select(xdid) %>% unique() 

options(scipen=7)
png('./figures/mConfByDayofWeek.png')
barchart(frequency~weekday, data=mConfWeekday.count,
         main='Medium Confidence Segment by Instances for all XDID',
         xlab='Day of Week', ylab='Frequency', col='deepskyblue3',
         ylim=c(0,400000), scales=list(y=list(at=seq(0,400000,50000))))
dev.off()

png('./figures/mConfByDayofWeekPercent.png')
barchart(percent~weekday, data=mConfWeekday.count,
         main='Medium Confidence Segment by Instances for all XDID',
         xlab='Day of Week', ylab='Percentage of Total Data', col='deepskyblue3',
         ylim=c(0,1), scales=list(y=list(at=seq(0,1,0.2))))
dev.off()
sum(mConfWeekday.count$frequency)



mConf.hour <- data.table(table(mConf.complete$xdid,mConf.complete$hour))
colnames(mConf.hour) <- c('xdid','hour','frequency')
mConf.hour$hour <- factor(mConf.hour$hour, levels=hourSeq)

mConfHour.count <- data.table(mConf.hour[,sum(frequency),by=c('hour')])
colnames(mConfHour.count) <- c('hour', 'frequency')
mConfHour.count$hour <- factor(mConfHour.count$hour, levels=hourSeq)
mConfHour.count$percent <- (mConfHour.count$frequency/length(traffic$tstamp))*100
sum(mConfHour.count$percent) # verifies validity of the data

png('./figures/mConf_ByHourofDay.png')
barchart(frequency~hour, data=mConfHour.count, 
         main='Medium Confidence Segment by Instances for all XDID',
         xlab='Hour of Day', ylab='Frequency', col='deepskyblue3',
         ylim=c(0,140000), scales=list(y=list(at=seq(0,140000,20000))))
dev.off()

png('./figures/mConf_ByHourofDayPercent.png')
barchart(percent~hour, data=mConfHour.count,
         main='Medium Confidence Segment by Instances for all XDID',
         xlab='Hour of Day', ylab='Percentage of Total Data', col='deepskyblue3',
         ylim=c(0,.4), scales=list(y=list(at=seq(0,.4,0.1))))
dev.off()


# spatially where are these segments occuring?

png('./figures/speedvslat_mconf.png', height=800, width=800)
xyplot(lat~speed, data=mConf, pch=16, col='deepskyblue3',
       main='Speed by Latitude', xlab='Speed (MPH)', ylab='Latitude (Degrees)',
       scales=list(x=list(at=seq(0,80,10)), y=list(at=seq(38,42,0.2))))
dev.off()

tmap_mode('view')
tm_shape(uniqueSegs) + tm_dots('position')


##### High confidence segments #####
paste0(round((length(hConf$xdid)/length(traffic$xdid))*100, 3), '% of the data')

length(unique(hConf$xdid))

# complete the data set
hConf$weekday <- weekdays(hConf$tstamp)
hConf$hour <- hour(hConf$tstamp)
hConf.complete <- left_join(tSeries, hConf, by=c('xdid','tstamp'))

pdf('./figures/hConf_Tseries.pdf')
xyplot(speed~tstamp | factor(xdid), data=hConf.complete, 
       type='l', col='deepskyblue3', layout=c(1,6), ylim=c(0,80), 
       scales=list(y=list(at=seq(0,80,20)))
)
dev.off()




# what is the temporal distribution through time? By day and hour.
hConf.weekday <- data.table(table(hConf.complete$xdid,hConf.complete$weekday))
colnames(hConf.weekday) <- c('xdid','weekday','frequency')
hConf.weekday$weekday <- factor(hConf.weekday$weekday, levels=weekdaySeq)

hConfWeekday.count <- data.table(hConf.weekday[,sum(frequency),by=c('weekday')])
colnames(hConfWeekday.count) <- c('weekday', 'frequency')
hConfWeekday.count$weekday <- factor(hConfWeekday.count$weekday, levels=weekdaySeq)
hConfWeekday.count$percent <- (hConfWeekday.count$frequency/length(traffic$tstamp))*100
sum(hConfWeekday.count$percent) # verifies validity of the data

png('./figures/hConfByDayofWeek.png')
barchart(frequency~weekday, data=hConfWeekday.count,
         main='High Confidence Segment by Instances for all XDID',
         xlab='Day of Week', ylab='Frequency', col='deepskyblue3',
         ylim=c(0,7000000), scales=list(y=list(at=seq(0,7000000,1000000))))
dev.off()

png('./figures/hConfByDayofWeekPercent.png')
barchart(percent~weekday, data=hConfWeekday.count,
         main='High Confidence Segment by Instances for all XDID',
         xlab='Day of Week', ylab='Percentage of Total Data', col='deepskyblue3',
         ylim=c(0,20), scales=list(y=list(at=seq(0,20,1))))
dev.off()





hConf.hour <- data.table(table(hConf.complete$xdid,hConf.complete$hour))
colnames(hConf.hour) <- c('xdid','hour','frequency')
hConf.hour$hour <- factor(hConf.hour$hour, levels=hourSeq)

hConfHour.count <- data.table(hConf.hour[,sum(frequency),by=c('hour')])
colnames(hConfHour.count) <- c('hour', 'frequency')
hConfHour.count$hour <- factor(hConfHour.count$hour, levels=hourSeq)
hConfHour.count$percent <- (hConfHour.count$frequency/length(traffic$tstamp))*100
sum(hConfHour.count$percent) # verifies validity of the data

png('./figures/hConf_ByHourofDay.png')
barchart(frequency~hour, data=hConfHour.count, 
         main='High Confidence Segment by Instances for all XDID',
         xlab='Hour of Day', ylab='Frequency', col='deepskyblue3',
         ylim=c(0,1750000), scales=list(y=list(at=seq(0,1750000,250000))))
dev.off()

png('./figures/hConf_ByHourofDayPercent.png')
barchart(percent~hour, data=hConfHour.count,
         main='High Confidence Segment by Instances for all XDID',
         xlab='Hour of Day', ylab='Percentage of Total Data', col='deepskyblue3',
         ylim=c(0,4.5), scales=list(y=list(at=seq(0,4.5,.5))))
dev.off()


# spatially where are these segments occuring?

png('./figures/speedvslat_hconf.png', height=800, width=800)
xyplot(lat~speed, data=hConf, pch=16, col='deepskyblue3',
       main='Speed by Latitude', xlab='Speed (MPH)', ylab='Latitude (Degrees)',
       scales=list(x=list(at=seq(0,80,10)), y=list(at=seq(38,42,0.2))))
dev.off()

tmap_mode('view')
tm_shape(uniqueSegs) + tm_dots('position')