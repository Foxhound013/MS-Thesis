library(lattice)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(RColorBrewer)
library(data.table)
# library(zoo)
# library(xts)

# this script is intended to deal with larger datasets in order to analyze their missing values.

fpath <- 'C:/Users/Downi/Desktop/speedData_May.csv'
traffic <- fread(input=fpath)

# fix up the data
traffic <- setDT(traffic)
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='UTC')
traffic <- complete(traffic, xdid, tstamp) # Complete may not be feasible for much larger data . . .

glimpse(traffic)
toy <- setDT(traffic[1:1000,])

# This method below works great for small data but will NOT work on this dataset.
#na_tseries <- traffic %>% group_by(xdid, tstamp) %>% transmute(count=sum(is.na(speed)))
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




