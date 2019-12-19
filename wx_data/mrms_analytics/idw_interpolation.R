library(ncdf4)
library(dplyr)
library(gstat)
library(sp)

start <- Sys.time()
#fpath <- 'C:/Users/Downi/Desktop/scratch/smaller_sub.nc'
setwd('/home/wdownin/thesis/MS-Thesis/wx_data/mrms_analytics')
file <- tail(commandArgs(), n=1) # example input from bash '6_out743'
fpath <- paste0('/depot/wwtung/data/loganD/wxData/mrms/archive/', file, '.nc')
#fpath <- '/depot/wwtung/data/loganD/wxData/mrms/archive/6_out743.nc'
mrms <- nc_open(fpath)

lon <- ncvar_get(mrms, 'longitude')
nlon <- dim(lon)
lat <- ncvar_get(mrms, 'latitude')
nlat <- dim(lat)

# manage the time componenet and check for missigness
time <- ncvar_get(mrms, 'time') # defines the number of loops for interpolation
tunits <- ncatt_get(mrms,'time','units')

timeSeq <- data.frame(tstamp=as.POSIXct(time, tz='utc', origin='1970-01-01 00:00:00'))

# variable information
varName <- 'PrecipRate_0mabovemeansealevel'
precipRate.units <- ncatt_get(mrms,varName, 'units')
fillvalue <- ncatt_get(mrms,varName,"_FillValue")

# get the road data and prep it.
roads <- read.csv('./data/processed/uniqueSegs_I65.csv')

roads.sp <- read.csv('./data/processed/uniqueSegs_I65.csv')
coordinates(roads.sp) <- ~lon+lat
proj4string(roads.sp) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
roads.sp <- spTransform(roads.sp, CRS('+init=epsg:32616'))

# can loop through the time by specifying where to start in the netcdf
pb <- txtProgressBar(min = 1, max = length(time), style = 3) 
final.interp <- data.frame()
i <- 1

while (i <= length(timeSeq$tstamp)) {
  # prep precip data
  precipRate <- ncvar_get(mrms,varName, start=c(1,1,i), count=c(411,451,1))
  precip.data <- expand.grid(lon=lon, lat=lat)
  precip.data$precip <- as.vector(precipRate)
  
  # convert data to spatial points dataframe.
  coordinates(precip.data) <- ~lon + lat
  proj4string(precip.data) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
  precip.data <- spTransform(precip.data, CRS('+init=epsg:32616'))
  
  # interpolate, create data frame, and join with road data.
  precip.interp <- idw(precip~1, precip.data, roads.sp, idp=2, maxdist=5000) %>% 
    spTransform(CRS('+init=epsg:4326')) %>% as.data.frame() %>% 
    transmute(precip=round(var1.pred, 2), lon=round(lon, 5), lat=round(lat, 5)) %>% 
    left_join(roads, by=c('lon', 'lat'))
  
  # cross validate and record MAE
  idw_cv <- krige.cv(precip~1, precip.data, set = list(idp = 2), maxdist=5000, nfold=3)
  precip.interp$MAE <- mean(abs(idw_cv$residual)) %>% round(2) # MAE
  
  final.interp <- bind_rows(final.interp, precip.interp)
  
  i <- i + 1
  setTxtProgressBar(pb, i)
  
}

nc_close(mrms)
end <- Sys.time(); end-start
out_path <- paste0('/depot/wwtung/data/loganD/wxData/mrms/interpolated/',file,'.csv')
write.csv(x=final.interp, file=out_path, row.names=F)
#write.csv(x=final.interp, file='./data/processed/tmp.csv', row.names=F)

