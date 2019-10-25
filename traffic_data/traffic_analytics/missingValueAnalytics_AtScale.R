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

# High confidence segments
summary(hConf)
length(unique(hConf$xdid))

xyplot(speed~tstamp, data=traffic)


toy <- lConf[,c('xdid', 'lat', 'lon')]
unique(toy)
toy <- st_as_sf(toy, coords=c('lon','lat'))
tmap_mode('view')
tm_shape(toy) + tm_dots('xdid')

xyplot(speed~tstamp | factor(xdid), data=hConf, layout=c(1,3,1))


pdf('./figures/speedsByConfidence.pdf')
xyplot(speed~tstamp | factor(xdid) + factor(score), data=toy, layout=c(1,2,1))
dev.off()


# Need to update the analysis from this point on since there is 0 missigness in the data now.

na_tseries <- traffic[,sum(is.na(speed)), by = c('xdid', 'tstamp')]
colnames(na_tseries) <- c('xdid', 'tstamp', 'isNA')
head(na_tseries, 10)


# # This figure takes too long to create. Aggregating the results first is necessary to plot
# pdf('./figures/missingnessByTime_May.pdf')
# xyplot(isNA~tstamp | factor(xdid), data=na_tseries, pch=16, col='deepskyblue3',
#        layout=c(1,6,length(unique(na_tseries$xdid))/6))
# dev.off()

# aggregate by id and hour
traffic$hour <- hour(traffic$tstamp)

missByID <- traffic[,sum(is.na(speed)), by=c('xdid', 'hour')]
colnames(missByID) <- c('xdid', 'hour', 'isNA')
glimpse(missByID)

xyplot(isNA~hour | factor(xdid), data=missByID, layout=c(1,6,1))
xyplot(isNA~hour, data=missByID)

# missingness for each time slice
missByT <- na_tseries[,sum(isNA),by=c('tstamp')]
colnames(missByT) <- c('tstamp', 'isNA')

xyplot(isNA~tstamp, data=missByT)

missByT$hour <- hour(missByT$tstamp)
xyplot(isNA~hour, data=missByT)

# missingness ratio
missByT$missingRatio <- (missByT$isNA/length(missByT$tstamp))*100
xyplot(missingRatio~tstamp, data=missByT, ylim=c(0,20))


# the systematic looking peaks are likely some rogue segments that were out for a large portion of the
# time frame. How can we look at this both spatially and temporally.




