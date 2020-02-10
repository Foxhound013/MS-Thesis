library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
library(randomForest)
library(sf)
library(tmap)

traffic <- fread('./data/processed/I-65N_WxSpd.csv')
# traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')

anyNA(traffic$precip)

road.map <- traffic %>% select(startmm,lon,lat) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()

traffic <- traffic %>% mutate(event=ifelse(precip > 0, yes=T, no=F))
segs <- traffic %>% select(position) %>% unique()

traffic$speedClass <- ifelse(traffic$speed > 65, yes='Normal',
                             no= ifelse(traffic$speed >= 55 & traffic$speed <= 65,
                                        yes='Minor Slow Down',
                                        no=ifelse(traffic$speed >= 45 & traffic$speed < 55,
                                                  yes='Moderate Slow Down',
                                                  no='Major Slow Down')
                             )
)

sub <- traffic %>% select(startmm, speed, precip, bearing, construction, hours, daylight, dayofweek)
sub$construction <- ifelse(sub$construction==T, yes='Construction', no='Non-Construction')
sub$construction <- as.factor(sub$construction)
sub$hours <- as.factor(sub$hours)
sub$daylight <- ifelse(sub$daylight==T, yes='Day', no='Night')
sub$daylight <- as.factor(sub$daylight)
sub$dayofweek <- as.factor(sub$dayofweek)
sub$speedClass <- as.factor(sub$speedClass)
sub$bearing <- as.factor(sub$bearing)

sub <- sub %>% filter(dayofweek != 'Saturday' & dayofweek != 'Sunday')

set.seed(100)
train <- sample(nrow(sub), 0.005*nrow(sub), replace=F)

trainSet <- sub[train,]
validSet <- sub[-train,]


# rfImpute is for imputing via a random forest.
# sub.impute <- rfImpute(speedClass ~ ., data=trainSet)

m1 <- randomForest(speedClass ~ precip+dayofweek+hours, data=trainSet)
m1

# oob.errors <- data.frame(Trees=rep(1:nrow(m1$err.rate), times=3),
#                          Type=rep(c('OOB', 'Major Slow Down', 'Minor Slow Down', 'Normal'), each=nrow(m1$err.rate)),
#                          Error=c(m1$err.rate[,])
#                          )


m2 <- lm(speed~ .,data=trainSet)
summary(m2)


cloud(startmm~speed*precip, data=trainSet, perspective=T)

wireframe(speed~startmm*precip, data=trainSet)

library(plotly)
plot_ly(x=trainSet$speed, y=trainSet$precip,
        marker=list(size=2))



library(e1071)

m3 <- svm(speed ~ ., data=trainSet)


