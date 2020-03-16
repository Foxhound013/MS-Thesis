library(data.table)
library(dplyr)
library(lattice); library(latticeExtra);
library(hexbin)
library(hextri)
library(ggplot2); library(ggforce); library(ggridges);
library(sf)
library(tmap)

traffic <- fread('./data/processed/I-65N_WxSpd.csv')
#traffic$tstamp <- as.POSIXct(traffic$tstamp, tz='utc', origin='1970-01-01 00:00:00')
# traffic$day <- mday(traffic$tstamp)
anyNA(traffic$precip)

road.map <- traffic %>% select(startmm, position,lon,lat) %>% unique() %>% st_as_sf(coords=c('lon','lat'))
tmap_mode('view')
tm_shape(road.map) + tm_dots()

traffic <- traffic %>% mutate(event=ifelse(precip > 0, yes=T, no=F))
segs <- traffic %>% select(position) %>% unique()

# MAY NEED TO REMOVE LATER BUT STRIP OUT 20s
traffic <- traffic %>% filter(score == 30)

# pdf('./figures/precipVspeed.pdf')
# xyplot(precip ~ speed | factor(position), data=traffic, pch=16, alpha=0.2,
#        layout=c(3,3))
# dev.off()

# add urban vs rural
traffic <- traffic %>% mutate(urban=ifelse(position >= 2 & position <= 15,
                                           yes='Louisville', 
                                           no=ifelse(position >= 189 & position <= 239,
                                                     yes='Indianapolis',
                                                     no=ifelse(position >= 440 & position <= 458,
                                                               yes='Northern Indiana',
                                                               no='Rural')
                                           )
)
)

traffic$construction <- ifelse(traffic$construction, yes='Construction', no='Non-Construction')
traffic$construction <- factor(traffic$construction,
                               levels=c('Non-Construction','Construction'),
                               ordered=T) 

traffic$dayofweek <- factor(traffic$dayofweek, 
                            levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                            ordered=T)

traffic$weekend <- ifelse(traffic$dayofweek == 'Saturday' | traffic$dayofweek == 'Sunday',
                          yes='Weekend', no='Weekday')
traffic$weekend <- factor(traffic$weekend,
                          levels=c('Weekday','Weekend'),
                          ordered=T)

# traffic$hours <- as.factor(traffic$hours)
# levels(traffic$hours)

traffic <- traffic %>% mutate(hour.range=ifelse(traffic$hours >= 4 & traffic$hours < 10,
                                           yes='Morning', 
                                           no=ifelse(traffic$hours >= 10 & traffic$hours < 16,
                                                     yes='Morning Rush',
                                                     no=ifelse(traffic$hours >= 16 & traffic$hours < 22,
                                                               yes='Afternoon Rush',
                                                               no='Evening')
                                           )
)
)
traffic$hour.range <- factor(traffic$hour.range,
                             levels=c('Morning', 'Morning Rush', 'Afternoon Rush', 'Evening'),
                             ordered=T)

precip.ranges <- c(0,0.01,2.5,5,10,20,30,40,50,60,70,80,150)
#precip.ranges <- c(0,0.01,2.5,5,10,150)
#c(0,0.01,2.5,5,10,20,40,60,80,100,140) #old ranges
traffic$rcat <- cut(traffic$precip,
                    breaks=precip.ranges, right=F, include.lowest=T)
uniqueRanges <- unique(traffic$rcat)


# drop out known crashes
c1 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-08 20:38:00' |
                                   .$tstamp == '2018-06-08 20:40:00' |
                                   .$tstamp == '2018-06-08 20:42:00' |
                                   .$tstamp == '2018-06-08 20:44:00' |
                                   .$tstamp == '2018-06-08 20:46:00' |
                                   .$tstamp == '2018-06-08 20:48:00' |
                                   .$tstamp == '2018-06-08 20:50:00' |
                                   .$tstamp == '2018-06-08 20:52:00' |
                                   .$tstamp == '2018-06-08 20:54:00' |
                                   .$tstamp == '2018-06-08 20:56:00' |
                                   .$tstamp == '2018-06-08 20:58:00' |
                                   .$tstamp == '2018-06-08 21:00:00' |
                                   .$tstamp == '2018-06-08 21:02:00' |
                                   .$tstamp == '2018-06-08 21:04:00' |
                                   .$tstamp == '2018-06-08 21:06:00' |
                                   .$tstamp == '2018-06-08 21:08:00') & 
                                  (.$startmm>=100 & .$startmm<=110)
                                 )
                       )
   ) %>% .$x
sub <- traffic[c1,]

c2 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-10 19:30:00' |
                                          .$tstamp == '2018-06-10 19:32:00' |
                                          .$tstamp == '2018-06-10 19:34:00' |
                                          .$tstamp == '2018-06-10 19:36:00' |
                                          .$tstamp == '2018-06-10 19:38:00' |
                                          .$tstamp == '2018-06-10 19:40:00' |
                                          .$tstamp == '2018-06-10 19:42:00' |
                                          .$tstamp == '2018-06-10 19:44:00' |
                                          .$tstamp == '2018-06-10 19:46:00' |
                                          .$tstamp == '2018-06-10 19:48:00' |
                                          .$tstamp == '2018-06-10 19:50:00' |
                                          .$tstamp == '2018-06-10 19:52:00' |
                                          .$tstamp == '2018-06-10 19:54:00' |
                                          .$tstamp == '2018-06-10 19:56:00' |
                                          .$tstamp == '2018-06-10 19:58:00' |
                                          .$tstamp == '2018-06-10 20:00:00') & 
                                         (.$startmm>=56 & .$startmm<=66)
                                       )
                             )
         ) %>% .$x

c3 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-12 06:00:00' |
                                          .$tstamp == '2018-06-12 06:02:00' |
                                          .$tstamp == '2018-06-12 06:04:00' |
                                          .$tstamp == '2018-06-12 06:06:00' |
                                          .$tstamp == '2018-06-12 06:08:00' |
                                          .$tstamp == '2018-06-12 06:10:00' |
                                          .$tstamp == '2018-06-12 06:12:00' |
                                          .$tstamp == '2018-06-12 06:14:00' |
                                          .$tstamp == '2018-06-12 06:16:00' |
                                          .$tstamp == '2018-06-12 06:18:00' |
                                          .$tstamp == '2018-06-12 06:20:00' |
                                          .$tstamp == '2018-06-12 06:22:00' |
                                          .$tstamp == '2018-06-12 06:24:00' |
                                          .$tstamp == '2018-06-12 06:26:00' |
                                          .$tstamp == '2018-06-12 06:28:00' |
                                          .$tstamp == '2018-06-12 06:30:00') & 
                                         (.$startmm>=62 & .$startmm<=72)
                                       )
                             )
         ) %>% .$x

c4 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-21 18:50:00' |
                                          .$tstamp == '2018-06-21 18:52:00' |
                                          .$tstamp == '2018-06-21 18:54:00' |
                                          .$tstamp == '2018-06-21 18:56:00' |
                                          .$tstamp == '2018-06-21 18:58:00' |
                                          .$tstamp == '2018-06-21 19:00:00' |
                                          .$tstamp == '2018-06-21 19:02:00' |
                                          .$tstamp == '2018-06-21 19:04:00' |
                                          .$tstamp == '2018-06-21 19:06:00' |
                                          .$tstamp == '2018-06-21 19:08:00' |
                                          .$tstamp == '2018-06-21 19:10:00' |
                                          .$tstamp == '2018-06-21 19:12:00' |
                                          .$tstamp == '2018-06-21 19:14:00' |
                                          .$tstamp == '2018-06-21 19:16:00' |
                                          .$tstamp == '2018-06-21 19:18:00' |
                                          .$tstamp == '2018-06-21 19:20:00') & 
                                         (.$startmm>=128 & .$startmm<=138)
                                       )
                             )
         ) %>% .$x

c5 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-21 17:30:00' |
                                          .$tstamp == '2018-06-21 17:32:00' |
                                          .$tstamp == '2018-06-21 17:34:00' |
                                          .$tstamp == '2018-06-21 17:36:00' |
                                          .$tstamp == '2018-06-21 17:38:00' |
                                          .$tstamp == '2018-06-21 17:40:00' |
                                          .$tstamp == '2018-06-21 17:42:00' |
                                          .$tstamp == '2018-06-21 17:44:00' |
                                          .$tstamp == '2018-06-21 17:46:00' |
                                          .$tstamp == '2018-06-21 17:48:00' |
                                          .$tstamp == '2018-06-21 17:50:00' |
                                          .$tstamp == '2018-06-21 17:52:00' |
                                          .$tstamp == '2018-06-21 17:54:00' |
                                          .$tstamp == '2018-06-21 17:56:00' |
                                          .$tstamp == '2018-06-21 17:58:00' |
                                          .$tstamp == '2018-06-21 18:00:00') & 
                                         (.$startmm>=172 & .$startmm<=182)
                                       )
                             )
         ) %>% .$x

c6 <- do(traffic, data.frame(x = which((.$tstamp=='2018-06-26 14:50:00' |
                                          .$tstamp == '2018-06-26 14:52:00' |
                                          .$tstamp == '2018-06-26 14:54:00' |
                                          .$tstamp == '2018-06-26 14:56:00' |
                                          .$tstamp == '2018-06-26 14:58:00' |
                                          .$tstamp == '2018-06-26 15:00:00' |
                                          .$tstamp == '2018-06-26 15:02:00' |
                                          .$tstamp == '2018-06-26 15:04:00' |
                                          .$tstamp == '2018-06-26 15:06:00' |
                                          .$tstamp == '2018-06-26 15:08:00' |
                                          .$tstamp == '2018-06-26 15:10:00' |
                                          .$tstamp == '2018-06-26 15:12:00' |
                                          .$tstamp == '2018-06-26 15:14:00' |
                                          .$tstamp == '2018-06-26 15:16:00' |
                                          .$tstamp == '2018-06-26 15:18:00' |
                                          .$tstamp == '2018-06-26 15:20:00') & 
                                         (.$startmm>=110 & .$startmm<=120)
                                       )
                             )
         ) %>% .$x

all_crashes <- c(c1,c2,c3,c4,c5,c6) %>% unique()
traffic <- traffic[-all_crashes,]

sum(traffic$precip > 0)/nrow(traffic) * 100


sub <- traffic[sample(nrow(traffic),10000),]

xyplot(startmm~speed | factor(dayofweek)*factor(hour.range)*factor(construction),
       data=sub, groups=event, pch=16, alpha=0.3, layout=c(7,1,1))

distro <- traffic
distro$event <- ifelse(distro$event==T, yes='Precipitable', no='Non-Precipitable')
distro$event <- factor(distro$event,
                       levels=c('Precipitable', 'Non-Precipitable'),
                       ordered=T)

sub <- distro[sample(nrow(distro), 10000),]

pdf('./figures/bulkDensity.pdf', width=12, height=8)
densityplot(~speed | weekend*hour.range*construction*urban,
            data=distro, groups=event, 
            layout=c(4,2), xlab='Speed (mph)',
            axis=axis.grid, alhpa=0.5,
            from=0, to=90, plot.points=F, auto.key=T)
dev.off()


# split the dataset into the region and construction subsets
# i.e. Indianapolis Construction and Indianapolis non-construction (if it exists)
# these smaller sets will clean up the plots in the end with fewer duplicated sections
# in the lattice facets
indy_construction <- distro %>% filter(urban=='Indianapolis' & construction=='Construction') 
indy_nonConstruction <- distro %>% filter(urban=='Indianapolis' & construction=='Non-Construction')

northernIN_construction <- distro %>% filter(urban=='Northern Indiana' & construction=='Construction') 
northernIN_nonConstruction <- distro %>% filter(urban=='Northern Indiana' & construction=='Non-Construction')

louisville_construction <- distro %>% filter(urban=='Louisville' & construction=='Construction')
louisville_nonConstruction <- distro %>% filter(urban=='Louisville' & construction=='Non-Construction')

rural_construction <- distro %>% filter(urban=='Rural' & construction=='Construction')
rural_nonConstruction <- distro %>% filter(urban=='Rural' & construction=='Non-Construction')


myData <- list(indy_construction, indy_nonConstruction, northernIN_construction, northernIN_nonConstruction,
               louisville_construction, louisville_nonConstruction, rural_construction, rural_nonConstruction)




# png('./figures/testqqmath.png')
# p <- useOuterStrips(qqmath(event~speed | hour.range*weekend,
#                            data=indy_nonConstruction, groups=event, as.table=T,
#                            layout=c(4,2), auto.key=T)
# )
# print(p)
# dev.off()


# xyplot(startmm~speed | hour.range*weekend,
#        data=indy_nonConstruction, groups=event, as.table=T,
#        layout=c(4,2)
#        )


plot_i <- 1
for (df in myData){
  if (dim(df)[1] == 0){
    next
  }
  
  # png(paste0('./figures/bulkECDF/bulkECDF', plot_i, '.png'), units='in', res=220, width=8.5, height=6)
  # p <- useOuterStrips(ecdfplot(~speed | hour.range*weekend,
  #                              data=df, groups=event, as.table=T,
  #                              strip.left=T,
  #                              par.strip.text=list(cex=.68),
  #                              layout=c(4,2), xlab='Speed (mph)',
  #                              main=paste0('Empirical Cumulative Distribution for ', df$urban[1], ' ', df$construction[1]),
  #                              axis=axis.grid, alhpa=0.5, xlim=c(0,85),
  #                              from=0, to=90, plot.points=F, auto.key=list(cex=0.73, columns=2),
  #                              scale=list(x=list(at=seq(0,85,10), cex=0.7, alternating=T),
  #                                         y=list(cex=0.7))
  #                              )
  #                     )
  # print(p)
  # dev.off()
  # 
  # png(paste0('./figures/bulkbwplot/bulkbwplot', plot_i, '.png'), units='in', res=220, width=8.5, height=6)
  # p <- useOuterStrips(bwplot(rcat~speed | hour.range*weekend,
  #                            data=df, axis=axis.grid, do.out=T, as.table=T,
  #                            par.settings = list(plot.symbol = list(pch = 16, cex=0.3, alpha=0.5)),
  #                            strip.left=T,
  #                            par.strip.text=list(cex=.68), layout=c(4,2),
  #                            main=paste0('Box and Whisker Plot for ', df$urban[1], ' ', df$construction[1]),
  #                            xlab='Speed (mph)', ylab='Precipitation Rate (mm/hr)',
  #                            cex=0.3, xlim=c(0,85),
  #                            scale=list(x=list(at=seq(0,85,10), cex=0.7),
  #                                       y=list(cex=0.7))
  #                            )
  #                     )
  # print(p)
  # dev.off()
  # 
  # df.summary <- df %>% group_by(rcat,weekend,hour.range,construction) %>% tally() #%>% filter(n <= 200)
  # 
  # png(paste0('./figures/sampleSizes/bulkSampleSizes', plot_i, '.png'), units='in', res=220, width=8.5, height=6)
  # p <- useOuterStrips(dotplot(rcat~n | hour.range*weekend, 
  #                             data=df.summary,
  #                             as.table=T,
  #                             axis=axis.grid,
  #                             pch=16, cex=0.5,
  #                             strip.left=T,
  #                             par.strip.text=list(cex=.68),
  #                             main=paste0('Sample Sizes for ', df$urban[1], ' ', df$construction[1], ' (<= 200)'),
  #                             xlab='Precipitation Rate (mm/hr)',
  #                             ylab='Sample Size',
  #                             layout=c(6,2),
  #                             xlim=c(0,200),
  #                             scales=list(x=list(at=seq(0,200,25), cex=.65),
  #                                         y=list(cex=0.65))
  #                             )
  #                     )
  # print(p)
  # dev.off()
  
  # png(paste0('./figures/QQ_Plots/QQ_Distros', plot_i, '.png'), units='in', res=220, width=8.5, height=6)
  # p <- useOuterStrips(qq(event~speed | hour.range*weekend,
  #                        data=df, as.table=T,
  #                        strip.left=T,
  #                        par.strip.text=list(cex=0.68),
  #                        layout=c(4,2), cex=0.3, xlab='Precipitable Speeds (mph)',
  #                        ylab='Non-Precipitable Speeds (mph)',
  #                        main=paste0('Quantile-Quantile Plot for ', df$urban[1], ' ', df$construction[1]),
  #                        axis= axis.grid, alpha=0.5, pch=16, xlim=c(0,90), ylim=c(0,90),
  #                        scale=list(x=list(at=seq(0,90,10), cex=0.7),
  #                              y=list(at=seq(0,90,10), cex=0.7))
  #                        )
  #                     )
  # print(p)
  # dev.off()
  
  # png(paste0('./figures/qq_math/QQ_normal', plot_i, '.png'), units='in', res=220, width=8.5, height=6)
  # p <- useOuterStrips(qqmath(~speed | hour.range*weekend,
  #                            data=df, groups=event, as.table=T,
  #                            strip.left=T,
  #                            par.strip.text=list(cex=0.68),
  #                            layout=c(4,2), cex=0.3, xlab='Normal Distribution',
  #                            ylab='Speed (mph)',
  #                            main=paste0('Traffic Speed Vs Normal Distribution for ', df$urban[1], ' ', df$construction[1]),
  #                            axis=axis.grid, alpha=0.5, pch=16, ylim=c(0,90),
  #                            scale=list(x=list(cex=0.7),
  #                                       y=list(at=seq(0,90,10), cex=0.7)
  #                                       )
  #                            )
  #                     )
  # print(p)
  # dev.off()
  
  png(paste0('./figures/qq_math/QQ_gamma', plot_i, '.png'), units='in', res=220, width=8.5, height=6)
  p <- useOuterStrips(qqmath(~speed | hour.range*weekend,
                             data=df, groups=event, as.table=T,
                             strip.left=T,
                             par.strip.text=list(cex=0.68),
                             layout=c(4,2), cex=0.3, xlab='Normal Distribution',
                             ylab='Speed (mph)',
                             main=paste0('Traffic Speed Vs Normal Distribution for ', df$urban[1], ' ', df$construction[1]),
                             distribution=function(x) {
                               qgamma(x, shape=1, rate=0.5)
                             },
                             axis=axis.grid, alpha=0.5, pch=16, ylim=c(0,90),
                             scale=list(x=list(cex=0.7),
                                        y=list(at=seq(0,90,10), cex=0.7)
                                        )
                             )
                      )
  print(p)
  dev.off()

  
  plot_i <- plot_i + 1
}


for (df in myData){
  if (dim(df)[1] == 0){
    next
  }
  
  for (i in c('Morning','Morning Rush', 'Afternoon Rush', 'Evening')) {
    
    tmp.precip.weekday <- df %>% filter(event == 'Precipitable' & hour.range==i & weekend=='Weekday')
    tmp.nonPrecip.weekday <- df %>% filter(event == 'Non-Precipitable' & hour.range==i & weekend=='Weekday')
    
    out <- ks.test(tmp.precip.weekday$speed, tmp.nonPrecip.weekday$speed, alternative='greater')
    print(paste0('Kolmogorov Smirnoff Test for ', df$urban[1], ' ', df$construction[1], 
                 ' hour range ', i, ' on a weekday yields: '))
    print(out)
    
    tmp.precip.weekend <- df %>% filter(event == 'Precipitable' & hour.range==i & weekend=='Weekend')
    tmp.nonPrecip.weekend <- df %>% filter(event == 'Non-Precipitable' & hour.range==i & weekend=='Weekend')
    
    out <- ks.test(tmp.precip.weekend$speed, tmp.nonPrecip.weekend$speed, alternative='greater')
    print(paste0('Kolmogorov Smirnoff Test for ', df$urban[1], ' ', df$construction[1], 
                 ' hour range ', i, ' on a weekend yields: '))
    print(out)
    
  }
}





# png('./figures/bulkECDF/bulkECDF%02d.png', units='in', res=220, width=8.5, height=6)
# ecdfplot(~speed | weekend*hour.range*construction*urban,
#          data=distro, groups=event, as.table=T,
#          par.strip.text=list(cex=.68),
#          layout=c(4,2), xlab='Speed (mph)',
#          axis=axis.grid, alhpa=0.5, xlim=c(0,85),
#          from=0, to=90, plot.points=F, auto.key=list(cex=0.73),
#          scale=list(x=list(at=seq(0,85,10))))
# dev.off()

sub <- traffic[sample(nrow(traffic),10000),]


# png('./figures/bulkbwplot/bulkbwplot%02d.png', units='in', res=220, width=8.5, height=6)
# bwplot(rcat~speed | weekend*hour.range*construction*urban, 
#        data=traffic, axis=axis.grid, do.out=T, as.table=T,
#        par.settings = list(plot.symbol = list(pch = 16, cex=0.3, alpha=0.5)),
#        par.strip.text=list(cex=.68), layout=c(4,2),
#        xlab='Speed (mph)', ylab='Precipitation Rate (mm/hr)',
#        cex=0.3, xlim=c(0,85),
#        scale=list(x=list(at=seq(0,85,10), cex=0.7),
#                   y=list(cex=0.7)
#                   )
#        )
# dev.off()

traffic.summary <- traffic %>% group_by(event, rcat, weekend, hour.range, construction, urban) %>% 
  summarize(n=n(),
            avgSpd=mean(speed),
            medianSpd=median(speed))

pdf('./figures/avgSpd_Bylocation.pdf', width=10, height=7)
xyplot(rcat~avgSpd | factor(hour.range)*factor(construction)*factor(weekend), data=traffic.summary,
       groups=urban, auto.key=T, cex=0.5, alpha=0.5,
       axis=axis.grid, ylab='Precipitation Intensity (mm/hr)',
       xlab='Average Speed (mph)',
       layout=c(4,2), xlim=c(40,80),
       scales=list(x=list(at=seq(35,85,5))))
dev.off()

png('./figures/medSpd_byLocation/medianSpd_Bylocation%02d.png', units='in', res=220, width=8.5, height=6)
xyplot(rcat~medianSpd | factor(hour.range)*factor(construction)*factor(weekend), data=traffic.summary,
       groups=urban, cex=0.5, alpha=0.5, pch=16,
       auto.key=list(cex=0.73),
       par.strip.text=list(cex=0.68, layout=c(4,1)),
       axis=axis.grid, ylab='Precipitation Intensity (mm/hr)',
       xlab='Median Speed (mph)',
       layout=c(4,2), xlim=c(40,80),
       scales=list(x=list(at=seq(35,85,5))))
dev.off()

# recalculate the above figures as % reductions
















# pull out just the Indy region
indy <- sub %>% filter(urban=='Rural')

xyplot(startmm~speed | factor(dayofweek)*factor(hours)*factor(construction),
       data=indy, groups=event, pch=16, alpha=0.3, layout=c(5,1,1))



pdf('./figures/positionVspeed_hours.pdf', width=10, height=8)

xyplot(startmm ~ speed | factor(dayofweek)*factor(hours)*factor(construction), 
       data=sub, ylab='Road Position', xlab='Speed (mph)', par.strip.text=list(cex=1),
       par.settings=list(layout.heights=list(strip=1)), groups=event,
       pch=16, alpha=0.2, grid=T, layout=c(3,2,1), cex=.5)

xyplot(speed ~ startmm | factor(dayofweek)*factor(hours)*factor(construction), 
       data=traffic, ylab='Road Position', xlab='Speed (mph)', par.strip.text=list(cex=1),
       par.settings=list(layout.heights=list(strip=1)), groups=event,
       pch=16, alpha=0.2, grid=T, layout=c(2,1), cex=.5)

dev.off()

hexbinplot(startmm ~ speed | factor(dayofweek)*factor(event)*factor(construction),
           data=sub, ylab='Road Position', xlab='Speed (mph)', bins=50,
           aspect=1, layout=c(7,2))


levelplot(speed~as.integer(tstamp)*startmm, data=sub)
colorRampPalette()

levelplot(speed~tstamp*startmm | factor(dayofweek)*factor(construction), data=sub, 
          aspect=1, 
          col.regions=colorRampPalette(c("pink", "red", "orange","blue", "green"))
)















# clustering to discriminate between standard speeds and reduced speeds.

# tmp <- kmeans(traffic$speed, 2)
# tmp$tot.withinss
# traffic$cluster <- tmp$cluster
# pdf('./figures/precipVspeed_clust.pdf')
# xyplot(speed ~ precip | factor(position), data=traffic, groups=cluster, 
#        pch=16, layout=c(3,3))
# dev.off()


# rather than use k means to determine thresholds, use 55 as a threshold
# this will be roughly 10% less than 65, a typical speed limit for most of I-65
# 55 is the lowest postest speed limit on I-65

traffic.slow <- traffic %>% filter(speed <= 55)
traffic.fast <- traffic %>% filter(speed > 55)

nrow(traffic.fast)/nrow(traffic) * 100
nrow(traffic.slow)/nrow(traffic) * 100

# slow speeds make up a very small percentage of the dataset

# tmp <- traffic.slow %>% filter(position==250)
# xyplot(speed~precip | factor(position), data=traffic.slow,
#        layout=c(1,1,1))




png('./figures/precipVspeed_hourOfDay.png')
xyplot(precip~speed | factor(hours), data=traffic)
dev.off()

pdf('./figures/precipVspeed_hour_density.pdf', width=30)
densityplot(~speed | factor(hours), data=traffic, plot.points=F, groups=,
            layout=c(24,1))
dev.off()


# pdf('./figures/precipVspeed_dayofweek.pdf')
# xyplot(precip~speed | factor(dayofweek), data=traffic, pch=16, col='deepskyblue3', alpha=0.5,
#        layout=c(7,1), par.strip.text=list(cex=.8))
# dev.off()











tmp <- traffic


#tmp$construction <- as.factor(tmp$construction)
#levels(tmp$construction) <- c('Non-Construction','Construction')



sub <- tmp[sample(nrow(tmp),10000),]



# levelplot(speed~position*hours | factor(dayofweek), data=tmp)

traffic.summary.r <- tmp %>% group_by(hour.range,weekend,urban,construction,rcat) %>% 
  summarise(min.r=min(speed), max.r=max(speed), median.r=median(speed), 
            avg.r=mean(speed), std.r=sd(speed), n.r=n())

traffic.baseline <- tmp %>% filter(precip == 0) %>% 
  group_by(hour.range,weekend,urban,construction,rcat) %>% 
  summarise(min.b=min(speed), max.b=max(speed), median.b=median(speed), 
            avg.b=mean(speed), std.b=sd(speed), n.b=n())

traffic.summary <- left_join(traffic.summary.r, traffic.baseline)
traffic.summary <- traffic.summary %>% tidyr::fill(median.b, avg.b)

# NOTE: THESE FILL IN LINES WILL HAVE TO CHANGE IF ANY NEW STATS ARE ADDED OR REGIONS.
# below was a highly manual method for filling NAs
# traffic.summary[seq(2,12),c('min.b','max.b','median.b', 'std.b', 'n.b')] <- traffic.summary[1,c('min.b','max.b','median.b', 'std.b', 'n.b')]


percent.reduction <- 100 - (traffic.summary$median.r / traffic.summary$median.b)*100
traffic.summary$median.percent.reduction <- percent.reduction %>% round(2)

traffic.summary$raw.reduction <- traffic.summary$median.r - traffic.summary$median.b

percent.reduction.avg <- 100 - (traffic.summary$avg.r / traffic.summary$avg.b)*100
traffic.summary$avg.percent.reduction <- percent.reduction.avg %>% round(2)


construction.summary <- traffic.summary %>% filter(construction=='Construction')
nonConstruction.summary <- traffic.summary %>% filter(construction=='Non-Construction')

myData <- list(construction.summary, nonConstruction.summary)
color1 <- '#648FFF'; color2 <- '#785EF0'; color3 <- '#DC267F'; color4 <- '#FE6100'; color5 <- '#FFB000'

# plot_i <- 1
# for (df in myData){
#   if (dim(df)[1] == 0){
#     next
#   }
#   
#   png(paste0('./figures/percent_reductions/median_percentReduction', plot_i, '.png'), 
#       units='in', res=220, width=8.5, height=6)
#   p <- useOuterStrips(  xyplot(rcat~median.percent.reduction | hour.range*weekend,
#                                data=df[which(df$n.r > 100),], groups=urban, alpha=0.7, cex=0.7, pch=c(16,15,17,18), 
#                                col=c(color1, color2, color3, color4), jitter=T,
#                                axis=axis.grid, ylab='Precipitation Intensity (mm/hr)',
#                                xlab='Median Speed (mph)', main='Median Speed Percent Reduction (Sample Sizes > 100)',
#                                par.strip.text=list(cex=0.68),
#                                auto.key=list(columns=4),
#                                # key=list(space='top',
#                                #          text=list(levels(factor(c('Indianapolis', 'Louisville', 'Northern Indiana', 'Rural')))),
#                                #          cex=0.9,
#                                #          points=list(pch=c(16,15,17,18), cex=0.7, col=c(color1, color2, color3, color4), alpha=0.7),
#                                #          columns=4
#                                #          ),
#                                as.table=T,
#                                strip.left=T,
#                                layout=c(4,2), xlim=c(-6,16),
#                                scales=list(x=list(at=seq(-10,20,2), rot=90, cex=0.7),
#                                            y=list(cex=0.7)
#                                            )
#                                )
#                         )
# 
#   
#   print(p)
#   dev.off()
# 
# 
#   
#   plot_i <- plot_i + 1
# }





png(paste0('./figures/speed_reductions/median_Reduction_Construction.png'), 
    units='in', res=220, width=8.5, height=6)
p <- useOuterStrips(  xyplot(rcat~raw.reduction | hour.range*weekend,
                             data=construction.summary[which(construction.summary$n.r > 100),], 
                             groups=urban, alpha=0.7, cex=0.7, pch=c(17,18), 
                             col=c(color3, color4), jitter=T,
                             axis=axis.grid, 
                             ylab='Precipitation Intensity (mm/hr)',
                             xlab='Speed (mph)', 
                             main='Median Speed Reduction for Construction (Sample Sizes > 100)',
                             par.strip.text=list(cex=0.68),
                             key=list(space='top',
                                      text=list(levels(factor(c('Northern Indiana', 'Rural')))),
                                      cex=0.9,
                                      points=list(pch=c(17,18), cex=0.7, 
                                                  col=c(color3, color4), 
                                                  alpha=0.7),
                                      columns=2
                                      ),
                             as.table=T,
                             strip.left=T,
                             layout=c(4,2), xlim=c(-11,5),
                             scales=list(x=list(at=seq(-10,4,2), cex=0.7),
                                         y=list(cex=0.7)
                                         )
                             )
                      )
print(p)
dev.off()

png(paste0('./figures/speed_reductions/median_Reduction_nonConstruction.png'), 
    units='in', res=220, width=8.5, height=6)
p <- useOuterStrips(  xyplot(rcat~raw.reduction | hour.range*weekend,
                             data=nonConstruction.summary[which(nonConstruction.summary$n.r > 100),], 
                             groups=urban, alpha=0.7, cex=0.7, pch=c(15,16,17,18), 
                             col=c(color1, color2, color3, color4), jitter=T,
                             axis=axis.grid, 
                             ylab='Precipitation Intensity (mm/hr)',
                             xlab='Speed (mph)', 
                             main='Median Speed Reduction for Non-Construction Zones (Sample Sizes > 100)',
                             par.strip.text=list(cex=0.68),
                             key=list(space='top',
                                      text=list(levels(factor(c('Indianapolis', 'Louisville', 'Northern Indiana', 'Rural')))),
                                      cex=0.9,
                                      points=list(pch=c(15,16,17,18), cex=0.7, 
                                                  col=c(color1, color2, color3, color4), 
                                                  alpha=0.7),
                                      columns=4
                             ),
                             as.table=T,
                             strip.left=T,
                             layout=c(4,2), xlim=c(-11,5),
                             scales=list(x=list(at=seq(-10,4,2), cex=0.7),
                                         y=list(cex=0.7)
                                         )
                             )
                      )
print(p)
dev.off()

# plot the average percent reduction and maybe sample sizes? The sample sizes before should be sufficient right?