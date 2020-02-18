library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
library(randomForest)
library(e1071)
library(gbm)
library(xgboost)
library(caret)
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

traffic <- traffic %>% group_by(startmm) %>% mutate(medianSpd=median(speed)) %>% ungroup()
traffic$speedClass <- ifelse(traffic$speed >= traffic$medianSpd,
                             yes='Normal', no='Slow Down')

traffic <- traffic %>% mutate(region=ifelse(position >= 2 & position <= 15,
                                            yes='Louisville', 
                                            no=ifelse(position >= 189 & position <= 239,
                                                      yes='Indianapolis',
                                                      no=ifelse(position >= 440 & position <= 458,
                                                                yes='Northern Indiana',
                                                                no='Rural')
                                            )
)
)

spd.ranges <- c(0,50,60,100)
traffic$rspd <- cut(traffic$speed, breaks=spd.ranges, right=F, include.lowest=T)

precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
traffic$rcat <- cut(traffic$precip,
                    breaks=precip.ranges, right=F, include.lowest=T)

# traffic$speedClass <- ifelse(traffic$speed > 55, yes='Normal',
#                              no= ifelse(traffic$speed >= 50 & traffic$speed <= 55,
#                                         yes='Minor Slow Down',
#                                         no=ifelse(traffic$speed >= 45 & traffic$speed < 50,
#                                                   yes='Moderate Slow Down',
#                                                   no='Major Slow Down')
#                              )
# )

traffic30 <- traffic %>% filter(score==30 & medianSpd >= 50 & region != 'Rural')


sub <- traffic %>% filter(score == 30) %>% 
  select(speed, precip, region, bearing, construction, 
         hours, daylight, dayofweek)


### ADD THE PRECIPITATION CATEGORIES IN TO THE DATASET


sub$region <- factor(sub$region)

sub$construction <- ifelse(sub$construction==T, yes='Construction', no='Non-Construction')
sub$construction <- factor(sub$construction)


sub <- sub %>% mutate(hour.range=ifelse(sub$hours >= 4 & sub$hours < 10,
                                        yes='Morning', 
                                        no=ifelse(sub$hours >= 10 & sub$hours < 16,
                                                  yes='Morning Rush',
                                                  no=ifelse(sub$hours >= 16 & sub$hours < 22,
                                                            yes='Afternoon Rush',
                                                            no='Evening')
                                        )
)
)
sub$hour.range <- factor(sub$hour.range,
                         levels=c('Morning', 'Morning Rush', 'Afternoon Rush', 'Evening'))

hour.arrangement <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14',
                      '15','16','17','18','19','20','21','22','23')
sub$hours <- factor(sub$hours, levels=hour.arrangement)

sub$daylight <- ifelse(sub$daylight==T, yes='Day', no='Night')
sub$daylight <- factor(sub$daylight)

sub$dayofweek <- factor(sub$dayofweek, 
                        levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

sub$weekend <- ifelse(sub$dayofweek == 'Saturday' | sub$dayofweek == 'Sunday',
                      yes='Weekend', no='Weekday')
sub$weekend <- factor(sub$weekend,
                      levels=c('Weekday','Weekend'))

#sub$speedClass <- factor(sub$speedClass)
sub$bearing <- factor(sub$bearing)

# sub$startmm <- factor(sub$startmm)


# sub2 <- sub %>% filter(startmm==113.46)

set.seed(42)
train <- sample(nrow(sub), 0.005*nrow(sub), replace=F)

trainSet <- sub[train,]
validSet <- sub[-train,]

str(sub)
# rfImpute is for imputing via a random forest.
# sub.impute <- rfImpute(speedClass ~ ., data=trainSet)




# m1 <- randomForest(rspd ~ ., data=trainSet, mtry=8, ntree=10)
# m1
# plot(m1)




# library(gbm)


traffic.boost <- gbm(speed ~ . ,data = trainSet, distribution = "gaussian",n.trees = 500,
                     shrinkage = 0.01, interaction.depth = 4)
summary(traffic.boost)

plot(traffic.boost, i='region')
plot(traffic.boost, i='construction')
plot(traffic.boost, i='hours')
plot(traffic.boost, i='precip')

n.trees = seq(from=100 ,to=10000, by=100) # number of trees

#Generating a Prediction matrix for each Tree
predmatrix<-predict(traffic.boost, validSet, n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(validSet,apply( (speed-predmatrix)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

plot(n.trees, test.error)









# library(xgboost)
# https://www.youtube.com/watch?v=woVTNwRrFHE


dummy <- dummyVars(speed~., data=trainSet)
train_mat <- predict(dummy, newdata=trainSet)
dummy <- dummyVars(speed~., data=validSet)
test_mat <- predict(dummy, newdata=validSet)

x_train <- xgb.DMatrix(train_mat)
y_train <- trainSet$speed

x_test <- xgb.DMatrix(test_mat)
y_test <- validSet$speed
  
xgb_trcontrol=trainControl(allowParallel=T)


xgb_model <- train(x_train, y_train, trControl=xgb_trcontrol, method='xgbTree')

predicted <- predict(xgb_model, x_test)
res <- y_test - predicted
root.mse = sqrt(mean(res^2))
mean.abs.error <- MAE(predicted, y_test)

# bstSparse <- xgboost(data = x_train, label = y_train, 
#                      max.depth = 2, eta = 1, nthread = 2, nrounds = 2, 
#                      objective = "binary:logistic")


xyplot(y_test[1:10000]~predicted[1:10000])


png('./figures/xgboost_residuals.png', width=10, height=7, units='in', res=300)
plot(res, pch=16)
dev.off()

densityplot(~res, plot.points=F,
            main='XGBoost Residuals Density Plot',
            xlab='Resdiual (Observed - Predicted)',
            xlim=c(-70,30),
            scales=list(x=list(at=seq(-70,30,10)))
)

densityplot(~res, plot.points=F,
            main='XGBoost Residuals Density Plot',
            xlab='Resdiual (Observed - Predicted)',
            xlim=c(-25,25),
            scales=list(x=list(at=seq(-25,25,5)))
            )

qqmath(res[1:100000],)


tmp <- validSet
tmp$predicted <- predicted
tmp$residual <- res

densityplot(~res | weekend*hour.range*construction*region, data=tmp,
            plot.points=F,
            layout=c(8,2))


tmp2 <- tmp %>% group_by(region) %>% summarise(prob1=quantile(residual, prob=0.1),
                                               prob2=quantile(residual, prob=0.2),
                                               prob3=quantile(residual, prob=0.3),
                                               prob4=quantile(residual, prob=0.4),
                                               prob5=quantile(residual, prob=0.5),
                                               prob6=quantile(residual, prob=0.6),
                                               prob7=quantile(residual, prob=0.7),
                                               prob8=quantile(residual, prob=0.8),
                                               prob9=quantile(residual, prob=0.9),
                                               min=min(residual),
                                               max=max(residual),
                                               mean=mean(residual),
                                               median=median(residual),
                                               count.less20 = sum(residual < -20))

# The above confirms the suspicion that rural regions are the primary contributor to majorly negative
# errors. The rural region and indianapolis region are not nearly as specific as the 
# northern indiana and louisville regions.





wireframe()

