library(ncdf4)
library(sp)
library(gstat)
library(parallel)


##### Load Data #####

#get mrms time
fpath <- 'C:/Users/Downi/Desktop/scratch/smaller_sub.nc'
mrms <- nc_open(fpath)
lon <- ncvar_get(mrms, 'longitude')
lat <- ncvar_get(mrms, 'latitude')
time <- ncvar_get(mrms, 'time')
varName <- 'PrecipRate_0mabovemeansealevel'
#preps a single time slice for precip.
# i <- 1
# precipRate <- ncvar_get(mrms,varName, start=c(1,1,i), count=c(411,451,1))
# precip.data <- expand.grid(lon=lon, lat=lat)
# precip.data$precip <- as.vector(precipRate)
nc_close(mrms)

# load static road data
roads.meta <- read.csv('../../traffic_data/traffic_analytics/data/processed/uniqueSegs_I65.csv')
roads <- read.csv('../../traffic_data/traffic_analytics/data/processed/uniqueSegs_I65.csv')
coordinates(roads) <- ~lon+lat
proj4string(roads) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
roads <- spTransform(roads, CRS('+init=epsg:32616'))

##### Begin Parallel #####
# Calculate the number of cores
num.cores <- detectCores() - 1

# Start Cluster
cl <- makeCluster(num.cores)

# split on timestep
parts <- split(x = 1:length(time), f = 1:num.cores)



krige.netcdf <- function() {
  
}







clusterExport(cl = cl, varlist = c("meuse", "meuse.grid", "m", "parts"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))

system.time(
  parallelX <- parLapply(cl = cl, X = 1:num.cores, 
                         fun = function(x) krige(formula = log(zinc)~1, 
                                                 locations = meuse, 
                                                 newdata = meuse.grid[parts[[x]],], 
                                                 model = m)
                         )
  )

stopCluster(cl)








#levelplot(precip~lon*lat, data=mrms)
# convert data to spatial points dataframe.
coordinates(mrms) <- ~lon + lat
proj4string(mrms) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
mrms <- spTransform(mrms, CRS('+init=epsg:32616'))




# Fit the variogram
# Note: Ordinary kriging => precip~1
# Universal kriging => precip~lon+lat
mrms.vgm <- variogram(precip~1, data=mrms)
mrms.vgm.fit <- fit.variogram(mrms.vgm, model=vgm('Sph', psill=1.6, nugget=1.5, range=100000))
mrms.kriged <- krige(precip~1, mrms, roads, model=mrms.vgm.fit, nmax=1000)




# recombine with road data
mrms.krige.wgs <- spTransform(mrms.kriged, CRS('+init=epsg:4326'))
mrms.krige.df <- cbind(coordinates(mrms.krige.wgs), 
                       mrms.krige.wgs$var1.pred, 
                       mrms.krige.wgs$var1.var) %>% as.data.frame
colnames(mrms.krige.df) <- c('lon', 'lat', 'pred', 'var')

roads.wx <- merge(roads.meta, mrms.krige.df, by=c('lon','lat'))

# correcting negatives
sum(roads.wx$pred < 0)
roads.wx[which(roads.wx$pred <= 0),'pred'] <- 0
roads.wx$var <- round(roads.wx$var, 2)
roads.wx$pred <- round(roads.wx$pred, 2)




