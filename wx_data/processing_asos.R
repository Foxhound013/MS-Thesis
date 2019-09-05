library(lattice)
library(tidyverse)

fpath <- 'C:/Users/Downi/Google Drive/Research/Thesis/wx_data/asos_april-july_2018.csv'
asos <- read.csv(fpath, stringsAsFactors = FALSE)

tslice <- asos[asos$valid == '4/1/2018 0:00',]
plot(tslice$vsby)

#! Need to set the variable types and column names. A lot of things are getting sorted as factors
# when they really shouldn't be.

str(asos)
summary(asos)

#! Fill missing values with na

levelplot(tslice$vsby~tslice$lat*tslice$lon)
levelplot(tslice$sped~tslice$lat*tslice$lon)

tslice2 <- asos[asos$valid == '4/1/2018 10:55',]

levelplot(tslice$vsby~tslice$lon*tslice$lat)
levelplot(tslice$sped~tslice$lon*tslice$lat)

tslice2$sped <- na_if(tslice2$sped, 'M')
tslice2$sped <- as.numeric(tslice2$sped)

plot(tslice$sped~tslice$lon*tslice$lat)

