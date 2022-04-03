rm(list=ls())
# these libraries are necessary
library(tree) 
library(randomForest)
library(data.table) # this library helps load big datasets faster (it's great)
# change paths if necessary

# these are the models from the paper
load("~/wristmethods/mods.RData")

# these are functions to estimate features, load data, etc
source("~/wristmethods/fuctions.for.models.R")

win.width <- 15 # width of "windows" to analyze in seconds

# ActiGraph file to load
file.1 <- "~/wristmethods/AgnMar01RAW.csv"


# Loading function asks for start time and stop times because sometimes the 
# monitor starts before subject is wearing it and stops after subject removes it. 
# The function only loads the necessary data. This can save time and memory.

# subject start time
start <- "18-Aug-14 7:00 AM"
start <- strptime(start,format="%d-%b-%y %I:%M %p")

# subject start time
stop <- "18-Aug-14 6:00 PM"
stop <- strptime(stop,format="%d-%b-%y %I:%M %p")

# this takes a little time for big files
ag.data <- read.act.2(file.1) 

n <- dim(ag.data)[1]

mins <- ceiling(n/(80*win.width)) 

# compute statistics (features)
ag.data$min <- rep(1:mins,each=win.width*80)[1:n]
ag.data$vm <- sqrt(ag.data$V1^2+ag.data$V2^2+ag.data$V3^2)
ag.data$v.ang <- 90*(asin(ag.data$V1/ag.data$vm)/(pi/2))
ag.data.sum <- data.frame(mean.vm=tapply(ag.data$vm,ag.data$min,mean,na.rm=T),
						sd.vm=tapply(ag.data$vm,ag.data$min,sd,na.rm=T),
						mean.ang=tapply(ag.data$v.ang,ag.data$min,mean,na.rm=T),
						sd.ang=tapply(ag.data$v.ang,ag.data$min,sd,na.rm=T),
						p625=tapply(ag.data$vm,ag.data$min,pow.625),
						dfreq=tapply(ag.data$vm,ag.data$min,dom.freq),
						ratio.df=tapply(ag.data$vm,ag.data$min,frac.pow.dom.freq))

# Next line can be slow... (there is a faster library, but I haven't implemented it yet.)
ag.data.sum$start.time <- as.POSIXlt(tapply(ag.data$time,ag.data$min,min,na.rm=T),origin="1970-01-01 00:00.00 UTC")

# apply the models (estimates are for each 15 second epoch)

# MET estimates by random forest
ag.data.sum$METs.rf <- predict(rf.met.model,newdata=ag.data.sum)
ag.data.sum$METs.rf[ag.data.sum$sd.vm<.01] <- 1

# MET estimates by linear regression
ag.data.sum$METs.lm <- predict(lm.met.model,newdata=ag.data.sum)
ag.data.sum$METs.lm[ag.data.sum$sd.vm<.01] <- 1

# MET level estimates (rf and tree)
ag.data.sum$MET.lev.rf <- predict(rf.met.level.model,newdata=ag.data.sum)
ag.data.sum$MET.lev.tr <- predict(tr.met.level.model,newdata=ag.data.sum,type="class")

# sedentary or not estimates (rf and tree)
ag.data.sum$sed.rf <- predict(rf.sed.model,newdata=ag.data.sum)
ag.data.sum$sed.tr <- predict(tr.sed.model,newdata=ag.data.sum,type="class")

# locomotion or not estimates (rf and tree)
ag.data.sum$loc.rf <- predict(rf.loc.model,newdata=ag.data.sum)
ag.data.sum$loc.tr <- predict(tr.loc.model,newdata=ag.data.sum,type="class")


