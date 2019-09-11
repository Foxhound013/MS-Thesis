library(lattice)
library(tidyverse)
library(rNOMADS)
library(sp)
library(raster)
library(fields)

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

fpath <- './asos_april-july_2018.csv'
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

## The results are that the asos data is pretty bad. Most of it is missing
# with the exception of visibility, wind speed, and direction.
# These are the only variables worth sourcing from asos.
##

asos_sub = asos[,c('station', 'valid_utc', 'lat', 'lon', 'vis', 'spd', 'drctn')]
# There is some strange values in the spd and and vis vars
asos_sub = asos_sub[which(asos_sub$vis <= 20),]
asos_sub = asos_sub[which(asos_sub$spd <= 75),]
summary(asos_sub)

# Let's try a lattice plot of histograms for the visibility data by station
# 
lattice::xyplot(vis~valid_utc | factor(station), data=asos_sub)

# what about if I restrict it down to April
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='04'))
# May?
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='05'))
# June?
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='06'))
# July?
lattice::xyplot(vis~valid_utc | factor(station), data=subset(asos_sub, format(valid_utc,'%m')=='07'))


# QQ plot between lafayette and Indy show a quadratic relationship. This makes some sense.
lattice::qq(station ~ vis, aspect = 1, data = asos_sub,
   subset = (asos_sub$station == "IND" | asos_sub$station == "LAF"))

# Let's look at some histograms!
lattice::histogram(~asos_sub$vis | factor(asos_sub$station), data=asos_sub)

# a majority of the data falls into the ten mile category, let's exclude the 10 mile data.
lattice::histogram(~asos_sub[which(asos_sub$vis < 10),'vis'] | factor(asos_sub$station), data=asos_sub)
summary(asos_sub[which(asos_sub$vis < 10),'vis'])

# most of the histograms look quite similar with the exception of PLD, HLB, and AID.
# Let's take a closer look at IND only.
ind <- asos_sub[which(asos_sub$station == 'IND'),]

lattice::histogram(~ind$vis)
lattice::xyplot(vis~valid_utc, data=ind)

# what if I condition this on hour of day? Maybe split out the datetime column into hour and day?
ind$hour <- sapply(ind$valid_utc, strftime, format='%H', tz='UTC')
asos_sub$hour <- sapply(asos_sub$valid_utc, strftime, format='%H', tz='UTC')


lattice::xyplot(vis~hour, data=ind)
plot(ind$hour, ind$vis)
lattice::histogram(~vis | factor(hour), data=ind)
# the 10 mile visibility still dominates the dataset across all hours. Let's exclude it.
lattice::histogram(~vis | factor(hour), data=ind[which(ind$vis < 10), ])


lattice::histogram(hour~vis, data=ind[which(ind$vis < 10),])

lattice::densityplot(~vis | factor(hour), data=ind[which(ind$vis < 10),])
lattice::densityplot(~vis | factor(hour), data=ind)


lattice::densityplot(~vis | factor(hour), data=asos_sub[which(asos_sub$vis < 10),])



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
