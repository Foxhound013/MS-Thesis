# This script has been written to pull together the traffic and precipitation data,
# manage imputation of that data, and prepare extra fields for analysis.
# This script will intake the traffic data as well as the weather data.
# additional features can be added fairly easily in post, but many of the 
# important ones have been built in here for convenience.

library(data.table)
library(dplyr)

traffic <- fread('../../../../../Desktop/June_I-65_2018.csv',
                 col.names=c('xdid', 'tstamp', 'speed', 'score', 'lat', 'lon', 'position',
                             'roadname', 'direction', 'bearing', 'startmm', 'endmm'))
traffic <- traffic %>% filter(direction=='N')

precip <- fread('../../../../../Desktop/6_out743.csv')
precip <- precip %>% filter(direction=='N')
precip$tstamp <- as.POSIXct(precip$tstamp, tz='utc', origin='1970-01-01 00:00:00')


# join the datasets after filtering to every 2 minutes and then impute missing precip data.
traffic <- traffic %>% mutate(tstamp=as.POSIXct(tstamp, tz='utc', origin='1970-01-01 00:00:00')) %>% 
  group_by(position) %>% filter((minute(tstamp)%%2)==0) %>% 
  left_join(precip, by=c('tstamp', 'lon', 'lat', 'position', 'xdid', 'direction')) %>% 
  do(zoo::na.locf(.))

anyNA(traffic$precip)

# develop additional features for the dataset

# characterize the dataset via time slots
traffic$hours <- hour(traffic$tstamp)
traffic$mins <- minute(traffic$tstamp)

# daylight 1000 to 0115 UTC
traffic$daylight <- NA
traffic[which(((traffic$hours>=1 & traffic$mins>=15) | (traffic$hours > 1 & traffic$mins >=0)) & 
                (traffic$hours < 10) ),'daylight'] <- F
traffic[is.na(traffic$daylight),'daylight'] <- T

traffic$dayofweek <- weekdays(traffic$tstamp) %>% 
  factor(levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

traffic$construction <- ifelse(traffic$startmm >= 8 & traffic$startmm <= 16 |
                                 traffic$startmm >= 50 & traffic$startmm <= 68 |
                                 traffic$startmm >= 141 & traffic$startmm <= 165 |
                                 traffic$startmm >= 167 & traffic$startmm <= 176 |
                                 traffic$startmm >= 197 & traffic$startmm <= 207 |
                                 traffic$startmm >= 229 & traffic$startmm <= 253,
                               yes=T, no=F)

write.csv(x=traffic, file='./data/processed/I-65N_WxSpd.csv', row.names=F)
