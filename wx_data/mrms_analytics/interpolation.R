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

# check completeness
# tmp <- data.frame(tstamp=as.POSIXct(time, tz='UTC', origin='1970-01-01'))
# start <- as.POSIXct('2018-04-01 00:00:00', tz='UTC', origin='1970-01-01')
# end <- as.POSIXct('2018-04-02 00:06:00', tz='UTC', origin='1970-01-01')
# date.seq <- seq.POSIXt(start, end, by='2 mins')
# tmp2 <- tmp %>% tidyr::complete(tstamp=date.seq)
# tibble::as_tibble(sub.missed)

varName <- 'PrecipRate_0mabovemeansealevel'
precipRate <- ncvar_get(mrms,varName)
precipRate.units <- ncatt_get(mrms,varName, 'units')
fillvalue <- ncatt_get(mrms,varName,"_FillValue")
nc_close(mrms)

# data structure is x, y, time
precip_tslice <- precipRate[,,1]
image(precip_tslice)
min(precip_tslice)
max(precip_tslice)

# create a dataframe of the data
mrms <- expand.grid(lon=lon, lat=lat)
mrms$precip <- as.vector(precip_tslice)

#levelplot(precip~lon*lat, data=mrms)
# convert data to spatial points dataframe.
coordinates(mrms) <- ~lon + lat
proj4string(mrms) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
mrms <- spTransform(mrms, CRS('+init=epsg:32616'))


# load data to fit to
roads.meta <- read.csv('../../traffic_data/traffic_analytics/data/processed/uniqueSegs_I65.csv')
roads <- read.csv('../../traffic_data/traffic_analytics/data/processed/uniqueSegs_I65.csv')
coordinates(roads) <- ~lon+lat
proj4string(roads) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
roads <- spTransform(roads, CRS('+init=epsg:32616'))

# as a matter of speeding things up, try buffering
# start <- Sys.time()
# #tmp <- rgeos::gBuffer(roads, width=15000)
# tmp <- st_buffer(st_as_sf(roads), dist=30000) %>% st_union() %>% st_intersection(st_as_sf(mrms))
# #plot(tmp)
# # plot(tmp, axes=T)
# # plot(roads, add=T)
# #tmp2 <- rgeos::intersect(mrms, tmp)
# intersected <- variogram(precip~1, data=tmp2)
# end <- Sys.time(); end-start

# plot(tmp)
# plot(mrms.vgm)
# intersected.fit <- fit.variogram(intersected, model=vgm('Sph')) # leaving parameters blank, gstat will sort them out
# plot(intersected, intersected.fit)

# Fit the variogram
start <- Sys.time()
# Note: Ordinary kriging => precip~1
# Universal kriging => precip~lon+lat
mrms.vgm <- variogram(precip~1, data=mrms)
#mrms.vgm <- variogram(precip~lon+lat, data=mrms) # takes some time
end <- Sys.time(); end-start # roughly 4 - 10 minutes
plot(mrms.vgm)
show.vgms()
mrms.vgm.fit <- fit.variogram(mrms.vgm, model=vgm('Sph', psill=1.6, nugget=1.5, range=100000)) # leaving parameters blank, gstat will sort them out

plot(mrms.vgm, mrms.vgm.fit)


# This is where a cross validation would be good to insert.
# Do one for ordinary and universal kriging.
# mrms.sub <- mrms[seq(1,10000),]
# ordk.cv <- krige.cv(precip~1, mrms.sub, model=mrms.vgm.fit, nfold=nrow(mrms), maxdist=5000)
# min(ordk.cv$residual); max(ordk.cv$residual)
# mean(abs(ordk.cv$residual))*100; min(abs(ordk.cv$residual)); max(abs(ordk.cv$residual))
#unvk.cv <- krige.cv(precip~lon+lat, locations=mrms, model=mrms.vgm)
start <- Sys.time()
mrms.kriged <- krige(precip~1, mrms, roads, model=mrms.vgm.fit, nmax=1000)
end <- Sys.time(); end-start
# mrms.kriged <- krige(precip~lon+lat, mrms, roads, model=mrms.vgm.fit, maxdist=2, nmax=100)

head(mrms.kriged)
plot(mrms.kriged$var1.pred)
spplot(mrms.kriged)


# recombine with road data
mrms.krige.wgs <- spTransform(mrms.kriged, CRS('+init=epsg:4326'))
mrms.krige.df <- cbind(coordinates(mrms.krige.wgs), 
                       mrms.krige.wgs$var1.pred, 
                       mrms.krige.wgs$var1.var) %>% as.data.frame
colnames(mrms.krige.df) <- c('lon', 'lat', 'pred', 'var')

roads.wx <- merge(roads.meta, mrms.krige.df, by=c('lon','lat'))

sum(roads.wx$pred < 0)
roads.wx[which(roads.wx$pred <= 0),'pred'] <- 0
roads.wx$var <- round(roads.wx$var, 2)
roads.wx$pred <- round(roads.wx$pred, 2)

roads.wx.sf <- st_as_sf(roads.wx, coords=c('lon','lat'))
tmap_mode('view')
tm_shape(roads.wx.sf) + tm_dots('pred', style='cont', palette=brewer.pal(9, 'YlGnBu'))
tm_shape(roads.wx.sf) + tm_dots('var', style='cont', palette=brewer.pal(9, 'YlGnBu'))

#mrms.kriged.sf <- st_as_sf(mrms.kriged)
#mrms.sf <- st_as_sf(mrms)
#tm_shape(mrms.sf) + tm_dots('precip', style='cont', palette=brewer.pal(9, 'YlGnBu'))
#tm_shape(mrms.kriged.sf) + tm_dots('var1.var', style='cont', palette=brewer.pal(9, 'YlGnBu'))

#plot(mrms.sf, axes=T)
# mrms.kriged.sf[which(mrms.kriged.sf$var1.pred <= 0),'var1.pred'] <- 0
# tm_shape(mrms.kriged.sf) + tm_dots('var1.pred', style='cont', palette=brewer.pal(9, 'YlGnBu'))
# plot(mrms.kriged.sf['var1.pred'], add=T, pch=16, key.pos=1, 
#      pal=colorRampPalette(brewer.pal(9, 'BuPu')))

















