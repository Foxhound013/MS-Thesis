library(ncdf4)
library(sp)
library(gstat)
library(lattice)

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



start <- Sys.time()
tmp <- idw(precip~1, mrms, roads, idp=5, maxdist=5000)#nmax=48)
end <- Sys.time(); end-start
spplot(tmp)

tmp.sf <- sf::st_as_sf(tmp)
tmap::tmap_mode('view')
tmap::tm_shape(tmp.sf) + tmap::tm_dots('var1.pred', style='cont')

tmp2 <- idw(precip~1, mrms, roads, idp=2, maxdist=10000)
tmp2.sf <- sf::st_as_sf(tmp2)
spplot(tmp2)

tmp3 <- tmp2.sf$var1.pred - tmp.sf$var1.pred %>% as.data.frame()
# this is quick enough that cross validation to choose the best power
# 1, 2, or 3 should be doable.

start <- Sys.time()
idw_cv <- krige.cv(precip~1, mrms, set = list(idp = 2), maxdist=5000, nfold=3)
end <- Sys.time(); end-start

# tmpry <- sf::st_as_sf(idw_cv)
mean(abs(idw_cv$residual)) #MAE


sub <- tmpry[1:10000,]

xyplot(observed~residual | factor(fold), data=sub)


































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


