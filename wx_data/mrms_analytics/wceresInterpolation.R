# options(java.parameters = "-Xmx4000m") # use this line when you have >100M data
source('/home/wdownin/rhinit.R')
library(datadr)
library(ncdf4)
library(sp)
library(gstat)
library(lattice)

mrms.dir <-  '/user/wdownin/wxData/smaller_sub.nc'
out.dir <- '/user/wdownin/wxData/processed/'

# load the metadata for mrms data
mrms <-  nc_open(mrms.dir)
lon <- ncvar_get(mrms, 'longitude')
nlon <- dim(lon)
lat <- ncvar_get(mrms, 'latitude')
nlat <- dim(lat)

time <- ncvar_get(mrms, 'time')
tunits <- ncatt_get(mrms,'time','units')

#map.values = list()
#map.keys = list()
#map.values[[1]] = rst[[1]][[2]]
#map.keys[[1]]=rst[[1]][[1]]
#r=1
# Map Code"