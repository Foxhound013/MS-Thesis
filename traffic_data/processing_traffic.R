library(lattice)
library(mhsmm)

fpath <- '/depot/wwtung/data/loganD/trafficData/m2_2017-10-24/trafficSpeeds.csv'
fig_dir <- '/depot/wwtung/data/loganD/figures/traffic/'


# unless you move your workflow over to hadoop, you can't work the whole file at once
# the only other option is to loop over reads.

##### Prep Data #####
# https://stackoverflow.com/questions/8772630/plotting-during-a-loop-in-rstudio
# link above regarding how to read csv in loops
# still not sure how to handle overreading the file
tmp <- read.csv(fpath, nrows=10, 
         colClasses=c('version'='factor', 'xdid'='integer', 'tstamp'='character',
                      'speed'='integer', 'roadname'='factor', 'roadname2'='factor',
                      'RoadNumber'='factor', 'direction'='factor', 'Bearing'='factor',
                      'County'='factor', 'District'='factor', 'StartLat'='numeric',
                      'StartLong'='numeric', 'Miles'='numeric'),
         stringsAsFactors=F)
# switch date to posixct


data <- read.csv(fpath,nrows=100000) # you can read in chunks with read.csv
object.size(data) # 0.7ish gb for 10Million rows

data$tstamp <- as.POSIXct.Date(data$tstamp, tz='UTC', origin='1970-01-01 00:00:00')
data$month <- strftime(data$tstamp, format='%m')
data$hour <- strftime(data$tstamp, format='%H')

##### Figures #####

hist(data$speed)
densityplot(data$speed)

speedQ <- quantile(data$speed, probs=seq(0,1,0.01))
plot(speedQ)
plot(gshape,gscale)

# gamma distribution attempt
gshape <- mhsmm::gammafit(data$speed)$shape
gscale <- mhsmm::gammafit(data$speed)$scale

plot(qgamma(seq(0,1,.01), gshape, scale=gscale), 
     pch=16, col='deepskyblue3')

probabilities = (1:length(data$speed))/(length(data$speed)+1)
gquants <- qgamma(probabilities, gshape, scale=gscale)

plot(sort(gquants), sort(data$speed))

lattice::qqmath(~speed | factor(District), data=data, pch=16,
                col='deepskyblue3', xlab='Speed (MPH)',
                distribution=function(x) qgamma(x, gshape, gscale))

# what happens if I only look at normal speeds for the gamma distribution
data.tmp <- subset(data, speed >=55 & speed <=75)

lattice::qqmath(~speed | factor(District), data=data.tmp, pch=16,
                col='deepskyblue3', xlab='Speed (MPH)',
                distribution=function(x) qgamma(x, gshape, gscale))



lognorm <- qlnorm(probabilities)
plot(sort(lognorm), sort(data$speed))


# Speed Factored by Month
pdf(paste0(fig_dir,'qq_speedFactoredByMonth.pdf', sep=''))
lattice::qqmath(~speed | factor(District), data=data, pch=16,
                col='deepskyblue3', xlab='Speed (MPH)')
dev.off()

# Speed Factored by Hour
pdf(paste0(fig_dir,'qq_speedFactoredByMonth2.pdf', sep=''))
lattice::qqmath(~speed | factor(month), data=data, pch=16,
                col='deepskyblue3', xlab='Speed (MPH)', layout=c(1,1))
dev.off()


plot(data$StartLong,data$StartLat)
