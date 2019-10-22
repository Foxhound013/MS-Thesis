library(ncdf4)
library(lattice)
library(sp)
library(gstat)

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

plot(order(precip_tslice))
plot(precip_tslice, pch=16)
grid <- expand.grid(lon=lon, lat=lat)
grid$precip <- as.vector(precip_tslice)

levelplot(precip ~ lon*lat, data=grid) # verifies that the data has been "gridded" properly

# convert your grid data to spatial points dataframe
coordinates(grid) <- ~lon+lat
summary(grid)

gridded(grid) = TRUE # make data spatial pixels dataframe
spplot(grid)
# Now to determine how to interpolate this time slice to a specific location

sample_points <- data.frame(lon=c(-87.3, -87.35), lat=c(40.56, 40.61))
coordinates(sample_points) <- ~lon+lat


# variogram will accept the spatial points dataframe but not the gridded version, throws
# error. Something about unequal grid

tmp <- variogram(precip~1, data=grid)
tmp.model <- vgm(psill=0.3, model='Gau', nugget=0.0001, range=5)
fitted <- fit.variogram(tmp, model=tmp.model)
plot(tmp, model=tmp.model)



