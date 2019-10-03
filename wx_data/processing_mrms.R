library(ncdf4)
library(lattice)
library(sp)
library(rgdal)
library(RColorBrewer)
library(data.table)
library(fields)
library(tidyverse)
library(gstat)
library(spacetime)

# useful resource http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

# Example data
fpath <- '/depot/wwtung/data/loganD/wxData/mrms/archive/smaller_sub.nc'
mrms <- ncdf4::nc_open(fpath)

lon <- ncdf4::ncvar_get(mrms, 'longitude')
nlon <- dim(lon)
lat <- ncdf4::ncvar_get(mrms, 'latitude')
nlat <- dim(lat)

time <- ncdf4::ncvar_get(mrms, 'time')
tunits <- ncdf4::ncatt_get(mrms,'time','units')

varName <- 'PrecipRate_0mabovemeansealevel'
precipRate <- ncdf4::ncvar_get(mrms,varName)
precipRate.units <- ncdf4::ncatt_get(mrms,varName, 'units')
fillvalue <- ncatt_get(mrms,varName,"_FillValue")
nc_close(mrms)

nPrecip <-  dim(precipRate)

# data structure is x, y, time
precip_tslice <- precipRate[,,1]
image(precip_tslice)
min(precip_tslice)
max(precip_tslice)
var(precip_tslice)

time <- as.POSIXct(time, origin='1970-01-01 00:00:00', tz='UTC')
# replace fill values with NAs
precipRate[precipRate==fillvalue$value] <- NA

# let's see what lattice stuff we can get up and running with this!
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(10,20,30,40,50,60,70,80,90,100)
levelplot(precip_tslice ~ lon * lat, data=grid, at=cutpts, cuts=10, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# investigate the distribution
hist(precip_tslice) # 0 dominates
lattice::qqmath(~as.vector(precip_tslice), ylab='Precipitation Intensity (mm/hr)')

# quantiles of all data
quantile(precip_tslice)
q = quantile(precip_tslice, probs=seq(0,1,.05))
plot(q)

precip_tslice.sub <- precip_tslice[which(precip_tslice > 0)]
quantile(precip_tslice.sub)
q = quantile(precip_tslice.sub, probs=seq(0,1,.05))
plot(q)

precip_tslice.sub <- precip_tslice[which(precip_tslice > 10)]
quantile(precip_tslice.sub)
q = quantile(precip_tslice.sub, probs=seq(0,1,.05))
plot(q)


## Look at one day of the dataset ##
dim(precipRate)
tmp <- precipRate[,,1:720]
summary(tmp)

tmp.sub <- tmp[which(tmp > 0)]
tmp.subq <- quantile(tmp.sub, probs=seq(0,1,0.05))
plot(tmp.subq)
# summary(precipRate) this will crash R

# No matter how it gets sliced and diced, it always seems to show a similar distro
# The large values are exceedingly rare while lower values are frequent. This will
# be even worse over time.

# One solution is to determine a threshold above which to search.
# Referencing my previous work, a good threshold would be 5.5 mm/hr
tmp.sub <- tmp[which(tmp >= 5.5)]
tmp.subq <- quantile(tmp.sub, probs=seq(0,1,0.05))
plot(tmp.subq)

# What happens if we do this with the full dataset
tmp.full <- precipRate[which(precipRate > 5.5), ,]
tmp.fullq <- quantile(tmp.full, probs=seq(0,1,0.01))
plot(tmp.fullq) # This works like a dream! 

plot(log(tmp.fullq))

summary(tmp.fullq)

# There is an interesting level off towards the end of the distro
plot(tmp.fullq[80:100])


# It looks like you've thrown off your dimensions.
as.data.frame(precipRate)



####################################
# Fresh
####################################
precip_tslice <- precipRate[,,1]
grid <- fields::make.surface.grid(list(lon,lat))
vals <- fields::as.surface(grid, precip_tslice)
image(vals)
# above shows that you officially have a surface for a time slice!

# If we wanted to go with bilinear interpolation, the following works great!
lon_want <- -86.3
lat_want <- 39
location_i <- matrix(c(lon_want, lat_want), nrow=1, ncol=2)
fields::interp.surface(vals, location_i)

# Kriging requires a variogram
# can't seem to get working

gstat::variogram(precip_tslice~1, location=vals, data=vals)



## Lets try getting a dataframe with all of the values like we had done with test
#slice <- data.frame(cbind(grid, time, as.vector(precip_tslice)))
test <- data.frame(expand.grid(lon,lat))
test$precip <- as.vector(precip_tslice)
colnames(test) <- c('lon','lat','precip')

summary(test)
precip_sp <- SpatialPoints(test,CRS("+init=epsg:4326"))

var <- variogram(precip~1, data=precip_sp)
plot(var)
