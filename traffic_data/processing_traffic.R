library(lattice)

fpath <- '/depot/wwtung/data/LoganD/trafficData/m2_2017-10-24/trafficSpeeds.csv'
# you can read in chunks with read.csv

data <- read.csv(fpath,nrows=500000)

lattice::qqmath(~speed | factor(District), data=data)


# Could you pipe this process? 