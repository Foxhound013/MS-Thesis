library(lattice)
library(tidyverse)
library(rNOMADS)
library(sp)
library(raster)
library(fields)

# Asos specs can be found at https://www.weather.gov/media/asos/aum-toc.pdf

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/wx_data/asos_april-july_2018.csv'
col_names = c('station', 'valid_utc', 'lon', 'lat', 'tmpc', 'dwptc', 'relh',
              'drctn', 'spd', 'precip_1hr_mm', 'vis', 'gust_mph', 'wxcodes')
asos <- read.csv(fpath, header=TRUE, stringsAsFactors = FALSE, col.names=col_names,
                 na.strings=c('M', '#NAME?', 'T'))

tslice <- asos[asos$valid_utc == '4/1/2018 0:00',]

coordinates(asos) <- ~lon+lat # create the spatial points df from asos data
rast <- raster(ncol=10, nrow=10)
extent(rast) <- extent(asos)

# below 
plot(rasterize(asos, rast, asos$vis, fun=mean))

# the above does rasterize your dataset.

##### Characterizing the ASOS Data #####

fpath <- '/depot/wwtung/data/loganD/wxData/asos/asos_april-july_2018_IN.csv'
col_names = c('station', 'valid_utc', 'lon', 'lat', 'tmpc', 'dwptc', 'relh',
              'drctn', 'spd', 'precip_1hr_mm', 'vis', 'gust_mph', 'wxcodes')
asos <- read.csv(fpath, header=TRUE, stringsAsFactors = FALSE, col.names=col_names,
                 na.strings=c('M', '#NAME?', 'T'))

asos$valid_utc <- as.POSIXct(asos$valid_utc, format="%m/%d/%Y %H:%M", tz="UTC")

# what is structure and summary of the data
str(asos)
summary(asos)

# How about missingness? How much of the data is missing for visibility?
vis.missing = (sum(is.na(asos$vis))/length(asos$vis)) * 100

# missingness of tmp?
tmpc.missing = (sum(is.na(asos$tmpc))/length(asos$tmpc)) * 100

# missingness of dpt?
dwptc.missing = (sum(is.na(asos$dwptc))/length(asos$dwptc)) * 100

# missingness of wind speed
spd.missing = (sum(is.na(asos$spd))/length(asos$spd)) * 100

# missingness of wind direction
drctn.missing = (sum(is.na(asos$drctn))/length(asos$drctn)) * 100

# missingness of precip
precip.missing = (sum(is.na(asos$precip_1hr_mm))/length(asos$precip_1hr_mm)) * 100

# gust mph
gust.missing = (sum(is.na(asos$gust_mph))/length(asos$gust_mph)) * 100

# wx codes
wxcodes.missing = (sum(is.na(asos$wxcodes))/length(asos$wxcodes)) * 100

missing_df = data.frame(vis.missing, tmpc.missing, dwptc.missing, dwptc.missing, 
           spd.missing, drctn.missing, gust.missing, precip.missing, wxcodes.missing)

missing_df
paste('Only the visibility, speed, and direction are any good.')
## The results are that the asos data is pretty bad. Most of it is missing
# with the exception of visibility, wind speed, and direction.
# These are the only variables worth sourcing from asos.
##

summary(asos$vis)
# There is a problem, with the asos data, no vis should be above 10 miles
asos <- asos[which(asos$vis <= 10 | is.na(asos$vis) ),]
summary(asos)

# Look at the quantiles for visibility across the whole dataset
vis.q <- quantile(asos$vis, probs=seq(0,.99,0.01), na.rm=T)
plot(vis.q)

#
lattice::histogram(~asos$vis | factor(asos$station), xlab="Visibility (Miles)")
# remove the 10 mile data to get a better view
asos.sub10 <- asos[which(asos$vis < 10 | is.na(asos$vis)),]
summary(asos.sub10)
lattice::histogram(~vis | factor(station), data=asos.sub10, xlab="Visibility (Miles)")

# Variation across site
lattice::qqmath(~vis | factor(station), data=asos, ylab="Visibility (Miles)", pch=16, cex=.5)

# Variation across hours?
asos$hour = strftime(asos$valid_utc, format='%H', tz='UTC')
lattice::qqmath(~vis | factor(hour), data=asos, ylab="Visibility (Miles)", pch=16, cex=.5)

# Look into stations for each time slice
stations <- asos[,c('station', 'valid_utc', 'lon', 'lat')]
station.freq <- as.data.frame(table(stations$station))
names(station.freq) <- c('station', 'freq')

unique_times <- length(unique(stations$valid_utc))

station.freq$percent <- station.freq$freq/unique_times * 100
plot(station.freq$station, station.freq$percent)

p <- barplot(station.freq$percent, names.arg=station.freq$station,
        cex.names=0.5, horiz=F)


# visibility has been thoroughly vetted now, but what about wind speed.
summary(asos$spd)
# The asos manual says speed doesn't report greater than 125 knots
asos.wind <- asos[which(asos$spd <= 125 | is.na(asos$spd)),]
summary(asos.wind$spd)

lattice::histogram(~asos.wind$spd | factor(asos.wind$station), xlab="Wind Speed (Knots)")
lattice::histogram(~asos.wind$spd | factor(asos.wind$hour), xlab="Wind Speed (Knots)")

# What about when compared to a distribution?
lattice::qqmath(~asos.wind$spd | factor(asos.wind$station), ylab="Wind Speed (Knots)", pch=16, cex=.5)
lattice::qqmath(~asos.wind$spd | factor(asos.wind$hour), ylab="Wind Speed (Knots)", pch=16, cex=.5)


# Now for wind direction
summary(asos.wind$drctn)
lattice::qqmath(~asos.wind$drctn)

lattice::histogram(~asos.wind$drctn | factor(asos.wind$station), xlab="Wind Direction (Degrees)")
lattice::histogram(~asos.wind$drctn | factor(asos.wind$hour), xlab="Wind Direction (Degrees)")


lattice::qqmath(~asos.wind$drctn | factor(asos.wind$station), ylab="Wind Speed (Knots)", pch=16, cex=.5)
lattice::qqmath(~asos.wind$drctn | factor(asos.wind$hour), ylab="Wind Speed (Knots)", pch=16, cex=.5)


# Let's get some general stats for presentation
asos2 <- asos[,c('station', 'valid_utc', 'lon', 'lat', 'drctn', 'spd', 'vis', 'hour')]
summary(asos)



# Seem to have exhausted possibilities for characterizing the data.
# Summary
# Visibility has good coverage at most sites. There is some weirdness at a few of the sites
# that may warrant discounting those locations. It seems if it's not an instrument error then
# it is a location error perhaps?
# If you can get the HRRR vis data with Brian Blaylock's Python code, for these sites, it would
# be possible to compare these visibilities with a qqplot to ensure they follow a similar distribution.

# The next big step in this exploration is attempting to regrid the asos data onto a regularized
# grid like what is available in MRMS in order to fuse the datasets.

##### Regrid #####
# fields::Krig() seems to be the function I'll want.
# interp.surface takes gridded data and interpolates to another grid. Maybe this can be done to get
# the MRMS and asos grids to match?

# find the unique lat and lon
locations <- data.frame(cbind(asos_sub$station, asos_sub$lat, asos_sub$lon))
names(locations) <- c('station','lat','lon')

locations <- unique(locations)
rownames(locations) <- seq(1,length(locations$station))


Krig(asos_sub$lon, asos_sub$lat, theta=20)

fields::as.image(asos_sub$vis, x = c(asos_sub$lon, asos_sub$lat))

tslice <- asos_sub[asos_sub$valid_utc == '2018-04-01 00:00:00',]

obj_a <- list(x= tslice$lon, y=tslice$lat, z=tslice$vis )
a.mat_a <- seq(from=38, to=42, by=0.5)
b.mat_a <- seq(from=-88,to=-86,by=0.5)
loc_a <- make.surface.grid(list(x=a.mat_a, y=b.mat_a))
xdata <- interp.surface(obj_a, loc_a)

krigSurface <- Krig(x=as.matrix(cbind(tslice$lon, tslice$lat)), Y=tslice$vis)
plot.Krig(krigSurface)
surface.Krig(krigSurface) # Just plots the krig surface

fields::as.surface(krigSurface)

krig.grid <- surface.Krig(krigSurface)


# Need to get a grid object from the mrms dataset
# then we can interpolate on to that grid.





# This shows that I may very well need to pull from other asos sites in other states
# in order to fit completely around Indiana. This isn't too much of a problem.

# lets try going spatial immediately instead
sp::coordinates(asos_sub) <- ~lon+lat
sp::proj4string(asos_sub) <- CRS("+init=epsg:4326")

bbox(asos_sub)

tslice <- asos_sub[asos_sub$valid_utc == '2018-04-01 00:00:00',]

plot(tslice)

grid <- sp::makegrid(tslice, n=1000)
plot(grid, pch='.')
plot(tslice, add=T)

ggplot(data = grid)+geom_point(aes(x=x1, y=x2))
