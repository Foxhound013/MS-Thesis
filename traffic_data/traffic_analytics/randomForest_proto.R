library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
library(randomForest)
library(e1071)
library(gbm)
library(xgboost)
library(caret)
library(DiagrammeR)
library(sf)
library(tmap)

traffic <- fread('./data/processed/I-65N_WxSpd.csv')
traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')
traffic <- traffic %>% group_by(startmm)
traffic <- traffic[order(traffic$tstamp),]
traffic <- traffic %>%  ungroup()


anyNA(traffic$precip)

road.map <- traffic %>% select(startmm,lon,lat) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()


segs <- traffic %>% select(position) %>% unique()


# Prep the traffic dataset
traffic <- traffic %>% mutate(event=ifelse(precip > 0, yes=T, no=F))
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

# add in a lagged speed, how many minutes, maybe 16 minutes? (i.e. 8 time steps)
# traffic$laggedSpeed <- traffic %>% group_by(startmm) %>% lag('speed', n=8)
traffic <- traffic %>% group_by(startmm) %>% 
  mutate(laggedSpeed=lag(speed,n=8)) %>% ungroup()

tmp <- traffic %>% filter(startmm==0.580)

# precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
# traffic$rcat <- cut(traffic$precip,
#                     breaks=precip.ranges, right=F, include.lowest=T)

# traffic30 <- traffic %>% filter(score==30)

sub <- traffic %>%
  select(speed, tstamp, startmm, laggedSpeed, precip, region, bearing, 
         construction, hours, daylight, dayofweek)
# removed region in favor of startmm for this test

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
                         levels=c('Morning', 'Morning Rush', 'Afternoon Rush', 'Evening'), ordered=T)



sub$daylight <- ifelse(sub$daylight==T, yes='Day', no='Night')
sub$daylight <- factor(sub$daylight)

sub$dayofweek <- factor(sub$dayofweek, 
                        levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

sub$weekend <- ifelse(sub$dayofweek == 'Saturday' | sub$dayofweek == 'Sunday',
                      yes='Weekend', no='Weekday')
sub$weekend <- factor(sub$weekend,
                      levels=c('Weekday','Weekend'))


sub$bearing <- factor(sub$bearing)

sub <- sub %>% select(-hours)

# 21 days is 70% of June
firstSeventy <- sub %>% group_by(startmm) %>% filter(tstamp < '2018-06-22 00:00:00') %>% 
  ungroup() %>% select(-tstamp, -startmm)
firstSeventy_info <- sub %>% group_by(startmm) %>% filter(tstamp < '2018-06-22 00:00:00') %>% 
  ungroup() %>% select(tstamp, startmm)

lastThirty <- sub %>% group_by(startmm) %>% filter(tstamp >= '2018-06-22 00:00:00') %>% 
  ungroup() %>% select(-tstamp, -startmm)
lastThirty_info <- sub %>% group_by(startmm) %>% filter(tstamp >= '2018-06-22 00:00:00') %>% 
  ungroup() %>% select(tstamp, startmm, region, construction, hour.range, weekend)


trainSet <- firstSeventy
validSet <- lastThirty


str(sub)





# https://www.youtube.com/watch?v=woVTNwRrFHE


dummy <- dummyVars(~., data=trainSet %>% select(-speed))
train_mat <- predict(dummy, newdata=trainSet %>% select(-speed))
dim(train_mat)

dummy <- dummyVars(~., data=validSet %>% select(-speed))
test_mat <- predict(dummy, newdata=validSet %>% select(-speed))

y_train <- trainSet$speed
x_train <- xgb.DMatrix(train_mat, label=y_train)

y_test <- validSet$speed
x_test <- xgb.DMatrix(test_mat, label=y_test)


# may need to register a parallel backend to get the parallel computation to work properly

parametersGrid <- expand.grid(nrounds=seq(200,1000,50),
                              max_depth=6,
                              eta=c(0.1, 0.2, 0.3),
                              gamma=0,
                              colsample_bytree=1,
                              min_child_weight=1,
                              subsample=1)

xgb_trcontrol=trainControl(method='cv',
                           number=3,
                           verboseIter=FALSE,
                           allowParallel=TRUE)

start <- Sys.time()
xgb_model <- train(x_train, y_train, trControl=xgb_trcontrol, method='xgbTree',
                   tuneGrid=parametersGrid)
end <- Sys.time(); end-start
# 
# start <- Sys.time()
# xgb_model <- xgb.train(params=parametersGrid,
#                        data=x_train,
#                        nrounds=500,
#                        print_every_n = 50)
# end <- Sys.time(); end-start
# 
# xgb_model <- xgboost(data=x_train, 
#                      eta=0.3, 
#                      max.depth=6, 
#                      colsample_bytree=0.5,
#                      nrounds=100,
#                      objective='reg:squarederror',
#                      nthread=11,
#                      verbose=1)
# 
png('./figures/xgboostCV.png', units='in', res=220, width=8.5, height=6)
plot(xgb_model)
dev.off()

xgb_model$bestTune



varImp(xgb_model)
summary(xgb_model)

xgb_imp <- xgb.importance(feature_names=xgb_model$finalModel$feature_names,
                          model=xgb_model$finalModel)

xgb_imp$Feature <- as.integer(xgb_imp$Feature)
featureNames <- colnames(train_mat)

xgb_imp$featureName <- featureNames[xgb_imp$Feature]

plot(xgb_imp$featureName, xgb_imp$Importance)
stripplot(featureName~Importance, data=xgb_imp)

# need to try summing up the importance of like categories now
summedImportance <- data.frame(region=sum(xgb_imp$Importance[c(1,4,9,10)]),
                               hours=sum(xgb_imp$Importance[c(15,16,17,18,21,22,24,25,26,27,
                                                              30,31,32,33,34,36,37,38,39)]),
                               day=sum(xgb_imp$Importance[c(11,14,19,23,28,29,35)]),
                               weekday_weekend=sum(xgb_imp$Importance[c(13,3)]),
                               day_night=sum(xgb_imp$Importance[c(5,6)]),
                               bearing=sum(xgb_imp$Importance[c(8,20)]),
                               precip=xgb_imp$Importance[7])
summedImportance <- reshape2::melt(summedImportance)
summedImportance <- summedImportance[order(summedImportance$value),]
rownames(summedImportance) <- NULL

summedImportance$variable <- factor(summedImportance$variable, levels=summedImportance$variable)
summedImportance$value <- round(summedImportance$value*100, 2)




barchart(variable~value, data=summedImportance,
         main='Feature Importance by Percentage',
         xlab='Percent Importance',
         axis=axis.grid,
         xlim=c(0,70),
         scales=list(x=list(at=seq(0,70,5))))

plot(xgb_model)

predicted <- predict(xgb_model$finalModel, x_test)
res <- y_test - predicted
root.mse = RMSE(predicted, y_test)
mean.abs.error <- MAE(predicted, y_test)
print(mean.abs.error)

summary(res)

output <- data.frame(speed=y_test,
                     predicted=predicted,
                     tstamp=lastThirty_info$tstamp,
                     startmm=lastThirty_info$startmm,
                     region=lastThirty_info$region,
                     construction=lastThirty_info$construction,
                     hour.range=lastThirty_info$hour.range,
                     weekend=lastThirty_info$weekend)

tmp <- output
tmp$tstamp <- as.POSIXct(tmp$tstamp, tz='utc', origin='1970-01-01 00:00:00')
# tmp$tstamp <- format(tmp$tstamp, tc='utc', usetz=T)
tmp <- tmp %>% group_by(startmm)
tmp <- tmp[order(tmp$tstamp),]
tmp_sub <- tmp %>% mutate(avg4min=frollmean(x=speed,n=2,fill=NA),
                          avg10min=frollmean(x=speed,n=5,fill=NA),
                          avg16min=frollmean(x=speed,n=8,fill=NA),
                          avg60min=frollmean(x=speed,n=30,fill=NA),
                          avg120min=frollmean(x=speed,n=60, fill=NA),
                          avg240min=frollmean(x=speed,n=120,fill=NA),
                          avg480min=frollmean(x=speed,n=480,fill=NA))


tmp_sub2 <- tmp_sub %>% filter(startmm == 130.56)
startEvent <- 1830 #which(tmp_sub2$tstamp == '2018-06-09 06:56:00')
endEvent <- 1880 #which(tmp_sub2$tstamp == '2018-06-09 17:00:00')
tmp_sub2 <- tmp_sub2[startEvent:endEvent,]
tmp_sub2$res <- tmp_sub2$speed - tmp_sub2$predicted


# tmp_sub <- tmp[203575 :6500,]

# xyplot(speed~tstamp, data=tmp_sub, type='l')

errorSummary <- tmp_sub %>% group_by(region, construction, hour.range, weekend) %>% 
  summarize(raw_mae=MAE(predicted, speed, na.rm=T),
            min10_mae=MAE(avg16min, speed, na.rm=T),
            min60_mae=MAE(avg60min, speed, na.rm=T))

lattice.blue <- "#0080ff"
lattice.pink <- "#ff00ff"
lattice.green <- 'darkgreen'

png('./figures/mae/nonConstruction_mae.png', units='in', res=220, width=8.5, height=6)
p <- useOuterStrips(dotplot(region~raw_mae+min10_mae+min60_mae | hour.range*weekend,
                            data=subset(errorSummary, errorSummary$construction=='Non-Construction'),
                            as.table=T, strip.left=T, axis=axis.grid,
                            main='MAE for Non-Construction Regions',
                            xlab='MAE (+/- mph)',
                            par.strip.text=list(cex=0.68),
                            pch=rep(16,3),
                            cex=0.7,
                            alpha=0.7,
                            layout=c(4,2),
                            key=list(columns=3, text=list(c('XGBoost', '10 min', '60 min')),
                                     points=list(pch=rep(16,3), 
                                                 col=c(lattice.blue, lattice.pink, lattice.green),
                                                 cex=0.7, alpha=0.7),
                                     cex=0.9
                            ),
                            xlim=c(0,5),
                            scales=list(x=list(at=seq(0,5,1), cex=0.7),
                                        y=list(cex=0.7))
)
)
print(p)
dev.off()


png('./figures/mae/construction_mae.png', units='in', res=220, width=8.5, height=6)
p <- useOuterStrips(dotplot(region~raw_mae+min10_mae+min60_mae | hour.range*weekend,
                            data=subset(errorSummary, errorSummary$construction=='Construction'),
                            as.table=T, strip.left=T, axis=axis.grid,
                            main='MAE for Construction Regions',
                            xlab='MAE (+/- mph)',
                            par.strip.text=list(cex=0.68),
                            pch=rep(16,3),
                            cex=0.7,
                            alpha=0.7,
                            layout=c(4,2),
                            key=list(columns=3, text=list(c('XGBoost', '10 min', '60 min')),
                                     points=list(pch=rep(16,3), 
                                                 col=c(lattice.blue, lattice.pink, lattice.green),
                                                 cex=0.7, alpha=0.7),
                                     cex=0.9
                            ),
                            xlim=c(0,5),
                            scales=list(x=list(at=seq(0,5,1), cex=0.7),
                                        y=list(cex=0.7))
)
)
print(p)
dev.off()




xyplot(speed+predicted+avg16min+avg60min~tstamp, data=tmp_sub2, type=c('l', 'p'),
       axis=axis.grid,
       ylab='Speed (mph)',
       xlab='Time Stamp (EST)',
       main='XGBoost Model Compared with Moving Averages',
       auto.key=list(columns=2, rows=2))




# xyplot(y_test[1:10000]~predicted[1:10000])

# MAYBE TRY HEXBIN PLOTTING THESE?
png('./figures/xgboost_residuals.png', width=10, height=7, units='in', res=300)
plot(res, pch=16)
dev.off()


resQuant <- quantile(res, probs=seq(0,1,0.01))
plot(seq(0,100), resQuant)

densityplot(~res, plot.points=F,
            main='XGBoost Residuals Density Plot',
            xlab='Resdiual (Observed - Predicted)',
            xlim=c(-75,45), axis=axis.grid,
            scales=list(x=list(at=seq(-75,45,10)))
)

densityplot(~res, plot.points=F,
            main='XGBoost Residuals Density Plot',
            xlab='Resdiual (Observed - Predicted)',
            xlim=c(-45,45), axis=axis.grid,
            scales=list(x=list(at=seq(-45,45,10)))
)




tmp <- validSet
tmp$predicted <- predicted
tmp$residual <- res




png('./figures/residualDensity.png', units='in', res=220, width=8.5, height=6)
densityplot(~res | region, data=tmp,
            plot.points=F, axis=axis.grid,
            layout=c(4,1))
dev.off()


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

# should probably normalize this by the number of segments in each region as this will have a major
# impact on the number of low residuals



tmp3 <- reshape2::melt(tmp2, id='region')
xyplot(value~variable, data=tmp3, groups=region)
