library(ncdf4)
library(lattice)
library(sp)
library(gstat)
library(sf)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(parallel)


fpath <- 'C:/Users/Downi/Desktop/scratch/smaller_sub.nc'
mrms <- nc_open(fpath)

lon <- ncvar_get(mrms, 'longitude')
nlon <- dim(lon)
lat <- ncvar_get(mrms, 'latitude')
nlat <- dim(lat)

time <- ncvar_get(mrms, 'time')
tunits <- ncatt_get(mrms,'time','units')

varName <- 'PrecipRate_0mabovemeansealevel'
precipRate <- ncvar_get(mrms,varName)
precipRate.units <- ncatt_get(mrms,varName, 'units')
fillvalue <- ncatt_get(mrms,varName,"_FillValue")
nc_close(mrms)

# data structure is x, y, time
precip_tslice <- precipRate[,,30]
image(precip_tslice)
min(precip_tslice)
max(precip_tslice)

# create a dataframe of the data
mrms <- expand.grid(lon=lon, lat=lat)
mrms$precip <- as.vector(precip_tslice)

levelplot(precip~lon*lat, data=mrms)
# convert data to spatial points dataframe.
coordinates(mrms) <- ~lon + lat
proj4string(mrms) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

# Fit the variogram
start <- Sys.time()
# Note: Ordinary kriging => precip~1
# Universal kriging => precip~lon+lat
mrms.vgm <- variogram(precip~lon+lat, data=mrms) # takes some time
end <- Sys.time(); end-start # roughly 4 - 10 minutes
plot(mrms.vgm)
show.vgms()
mrms.vgm.fit <- fit.variogram(mrms.vgm, model=vgm('Sph')) # leaving parameters blank, gstat will sort them out

plot(mrms.vgm, mrms.vgm.fit)

# load data to fit to
roads <- read.csv('../../traffic_data/traffic_analytics/data/processed/uniqueSegs_I65.csv')
coordinates(roads) <- ~lon+lat
proj4string(roads) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

mrms.kriged <- krige(precip~lon+lat, mrms, roads, model=mrms.vgm.fit, maxdist=2, nmax=100)

head(mrms.kriged)
sp::plot(mrms.kriged$var1.pred)
image(mrms.kriged$var1.pred)

mrms.kriged.sf <- st_as_sf(mrms.kriged)
mrms.sf <- st_as_sf(mrms)

tmap_mode('view')
tm_shape(mrms.sf) + tm_dots('precip', style='cont', palette=brewer.pal(9, 'YlGnBu'))
tm_shape(mrms.kriged.sf) + tm_dots('var1.pred', style='cont', palette=brewer.pal(9, 'YlGnBu'))

plot(mrms.sf, axes=T)
plot(mrms.kriged.sf['var1.pred'], add=T, pch=16, key.pos=1, 
     pal=colorRampPalette(brewer.pal(9, 'BuPu')))


# recombine with road data
tmp <- cbind(mrms.kriged.sf, st_coordinates(mrms.kriged.sf)) %>% as.data.frame
colnames(tmp) <- c('pred', 'var', 'lon', 'lat', 'geometry')
tmp2 <- merge(roads, tmp, by=c('lon','lat'))
# agregate precip into intervals, here 
tmp <- (precipRate[,,1] + precipRate[,,2])/2


# Speeding up variogram/kriging

# variogram function is extremely slow. Two options to 











