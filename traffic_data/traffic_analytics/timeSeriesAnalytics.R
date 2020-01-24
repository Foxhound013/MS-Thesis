library(data.table)
library(dplyr)
library(lattice)

traffic <- fread('./data/processed/I-65N_WxSpd.csv')
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')
anyNA(traffic$precip)
# order by tstamp
traffic <- traffic %>% group_by(position)
traffic <- traffic[do.call(order,traffic),]

road.map <- traffic %>% select(position,lon,lat,startmm,endmm) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()


# prototype off of one known event
tmp <- traffic %>% filter(mday(tstamp) == 9 & position == 238 & hours >= 16 & hours < 17)
# 4 minute moving average to calculate major speed drops.
tmp <- tmp %>% mutate(avgSpd=frollmean(x=tmp$speed,n=2,fill=NA))

xyplot(speed+avgSpd~tstamp, data=tmp, type=c('p','l','g'), auto.key=T)

# event detection
tmp <- tmp %>% mutate(event=ifelse( (100-(speed/avgSpd)*100) >= 5, yes=T, no=F),
                      pct_drop=100-(speed/avgSpd)*100)
xyplot(speed+avgSpd~tstamp, data=tmp, type=c('p','l','g'), auto.key=T, groups=event)

# window building - this test will use a smaller window than production

                      