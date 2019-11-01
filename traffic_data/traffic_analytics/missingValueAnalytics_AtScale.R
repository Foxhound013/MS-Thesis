library(lattice)
library(dplyr)
library(tidyr)
library(sf)
library(tmap); tmap_mode('view');
library(RColorBrewer)
library(data.table)
# library(zoo)
# library(xts)

# this script is intended to deal with larger datasets in order to analyze their missing values.

fpath <- 'C:/Users/Downi/Desktop/2018-04-25_I65_May.csv' # can't read from G-drive, causes issues
traffic <- fread(input=fpath)

# fix up the data
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='UTC')
#traffic <- complete(traffic, xdid, tstamp) # Complete may not be feasible for much larger data . . .
# 39,283,200 obs before complete
# 39,283,200 obs after complete 
# i.e. no missing data! :D But how many for each confidence bin?
traffic <- setDT(traffic)

glimpse(traffic)

# How many segments are there in total
totalSegs <- length(unique(traffic$xdid))

allSegs <- traffic[,c('xdid','lon','lat')] %>% unique %>% st_as_sf(coords=c('lon','lat'))
tm_shape(allSegs) + tm_dots()

# segment t series by confidence level
hConf <- traffic[which(score==30),]
mConf <- traffic[which(score==20),]
lConf <- traffic[which(score==10),]

length(hConf$xdid)/length(traffic$xdid)
length(mConf$xdid)/length(traffic$xdid)
length(lConf$xdid)/length(traffic$xdid)

# Low confidence segments
summary(lConf)
length(unique(lConf$xdid)) # only 3 segments are poor quality of the 880 total
unique(lConf$xdid)

# png('./figures/lconfSegs.png')
# xyplot(speed~tstamp | factor(xdid), data=lConf, layout=c(1,3,1), xlab='Time Stamp', 
#        ylab='Speed', main='Speed Time Series For Low Confidence Segments',
#        ylim=seq(0,75,10), pch=16, col='deepskyblue3',
#        scales=list(x=list(at=) ) )
# dev.off()

# locate the segments spatially
lConf2 <- st_as_sf(unique(lConf[,c('xdid', 'lon', 'lat')]), coords=c('lon','lat'))
tm_shape(lConf2) + tm_dots('xdid', style='cat')

# describe segments by time
lConf$weekday <- weekdays(lConf$tstamp)
weekdaySeq <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
hourSeq <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14',
             '15', '16', '17', '18', '19', '20', '21', '22', '23')

lConf.weekday <- data.frame(table(lConf$xdid, lConf$weekday))
colnames(lConf.weekday) <- c('xdid','weekday', 'frequency')
lConf.weekday$weekday <- factor(lConf.weekday$weekday, levels=weekdaySeq)

png('./figures/lConf_Frequency_ByDayOfWeek.png')
barchart(frequency~weekday | factor(xdid), data=lConf.weekday, ylab='Frequency',
         main='Frequency of Low Confidence Occurrences by Day of Week and XDID', 
         ylim=c(0,2000), scales=list(x=list(rot=90) ), layout=c(1,3,1) )
dev.off()


png('./figures/lconfByDayofWeek.png')
xyplot(speed~tstamp | factor(xdid) + factor(weekday, levels=weekdaySeq), ylim=seq(0,70,10),
       data=lConf, layout=c(3,7), pch=16, col='deepskyblue3', par.strip.text=list(cex=.7),
       main='Speeds By Day of Week and Segment ID', xlab='Time Stamp', ylab='Speed')
dev.off()


# describe by hour
lConf$hour <- hour(lConf$tstamp)
lConf.hour <- data.frame(table(lConf$xdid, lConf$hour))
colnames(lConf.hour) <- c('xdid','hour','frequency')

png('./figures/lConf_Frequency_ByHour.png')
barchart(frequency~hour | factor(xdid), data=lConf.hour, ylab='Frequency', xlab='Hour',
         main='Frequency of Low Confidence Occurrences by Day of Week and XDID', 
         layout=c(1,3,1) , ylim=c(0,2000))
dev.off()


# Medium confidence segments
summary(mConf)
length(unique(mConf$xdid)) # all segments are subject to medium confidence

mConf$weekday <- weekdays(mConf$tstamp)
mConf.weekday <- data.table(table(mConf$xdid, mConf$weekday))
colnames(mConf.weekday) <- c('xdid','weekday', 'frequency')
mConf.weekday$weekday <- factor(mConf.weekday$weekday, levels=weekdaySeq)

mConfWeekday.count <- data.table(mConf.weekday[,sum(frequency),by=c('weekday')])
colnames(mConfWeekday.count) <- c('weekday', 'count')
mConfWeekday.count$weekday <- factor(mConfWeekday.count$weekday, levels=weekdaySeq)

options(scipen=7)
png('./figures/mConfByDayofWeek.png')
barchart(count~weekday, data=mConfWeekday.count, ylim=seq(0,500000,500),
         main='Medium Confidence Segment by Instances for all XDID',
         xlab='Day of Week', ylab='Frequency')
dev.off()

mConf$hour <- hour(mConf$tstamp)
mConf.hour <- data.table(table(mConf$xdid, mConf$hour))
colnames(mConf.hour) <- c('xdid','hour', 'frequency')

mConfHour.count <- data.table(mConf.hour[,sum(frequency),by=c('hour')])
colnames(mConfHour.count) <- c('hour', 'count')
mConfHour.count$hour <- factor(mConfHour.count$hour, levels=hourSeq)

png('./figures/mConf_ByHourofDay.png')
barchart(count~hour, data=mConfHour.count,
         main='Medium Confidence Segment by Instances for all XDID', xlab='Hour',
         ylab='Frequency')
dev.off()

# what do the speeds look like
# good idea, poor execution. Below will give you an average across the whole month (not great)
mConf.avgSpd <- mConf[,mean(speed), by=c('xdid')]
summary(mConf.avgSpd)

# Does a random sample of the medium traffic speeds look comparable to that of the high?
mConf.groups <- mConf[,speed, by=c('xdid', 'tstamp')]

pdf('./figures/mConf_Tseries.pdf')
xyplot(speed~tstamp | factor(xdid), data=mConf.groups, type='l', pch=16, col='deepskyblue3',
       layout=c(1,6), ylim=seq(0,70,10))
dev.off()

# High confidence segments
summary(hConf)
length(unique(hConf$xdid))

# https://stats.stackexchange.com/questions/7348/statistical-methods-to-more-efficiently-plot-data-when-millions-of-points-are-pr
# above link may provide some useful tools for plotting this data

# there is too much data to straight out plot it, cut the t series down
# this may simply be a case of needing to submit to a cluster and wait for the results
xdid.hConf.unique <- unique(hConf[,'xdid'])
xdid.rand <- hConf[sample(nrow(xdid.hConf.unique), size = 100, replace=F),'xdid']

hConf.sub <- merge(hConf, xdid.rand)
hConf.sub <- complete(hConf.sub, xdid, tstamp) # fill in NA values

pdf('./figures/hConf_100RandomSamples.pdf', width=16)
xyplot(speed~tstamp | factor(xdid), data=hConf.sub, type='l', pch=16, col='deepskyblue3',
       layout=c(1,6), ylim=seq(0,70,10))
dev.off()



##### Smoothing the data #####
# start with a toy (small) example
xdid.unique <- unique(traffic[,'xdid'])
xdid.rand <- traffic[sample(nrow(xdid.unique), size = 1, replace=F),'xdid']
toy <- merge(traffic, xdid.rand)
toy.sub <- subset(toy, tstamp>=as.POSIXct('2018-05-01 00:00:00') & tstamp < '2018-05-02 00:00:00')

plot(toy.sub$tstamp, toy.sub$speed, type='l')

toy$tstamp <- as.integer(toy$tstamp)

toy.loess <- loess(speed~tstamp, data=toy, span=0.02) # roughly 20 seconds
newTimes <- seq(as.POSIXct('2018-05-01 00:00:00'),as.POSIXct('2018-05-01 23:59:00'), by = '15 mins') %>% as.integer
tmp <- predict(toy.loess, data.frame(tstamp=newTimes), se=FALSE)

plot(as.integer(toy$tstamp), toy$speed, type='l', cex=.5)
lines(as.integer(toy$tstamp), toy.loess$fitted, col='red')



##### Subset to 15 min #####

# Loess examples have been kept above for potential future use, but 15 min. bins will be explored
tmp <- toy %>% group_by(xdid,tstamp=cut(tstamp,breaks='15 min')) %>% summarise(avgSpeed=mean(speed))
plot(tmp$tstamp, tmp$avgSpeed, type='l')
lines(tmp$tstamp, tmp$avgSpeed)


# try this for the whole dataset, can it be done reasonably?
traffic.sub <- traffic %>% group_by(xdid,tstamp=cut(tstamp,breaks='15 min'))  %>% summarise(avgSpeed=mean(speed))
# surprisingly fast! Less than 1 minute
# significantly reduces data size as well

traffic.sub$tstamp <- as.POSIXct(traffic.sub$tstamp)
xyplot(avgSpeed~tstamp | factor(xdid), data=traffic.sub, type='l', layout=c(1,6,1))

bwplot(~avgSpeed | factor(xdid), data=traffic.sub, layout=c(1,1,1), horizontal=F)


traffic.1 <- traffic.sub[which(traffic.sub$xdid=='138360227'),]
xyplot(avgSpeed~tstamp, data=traffic.1, type='l')


