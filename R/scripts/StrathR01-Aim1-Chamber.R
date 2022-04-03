rm(list=ls())
path <- "/Users/jstauden/OneDrive - University of Massachusetts/SHARED_AIM1_MERGED_DATA/"
load("~/wristmethods/mods.RData")
source("~/wristmethods/fuctions.for.models.R")
# load the required neural networks
setwd('~/SojournCode')
wd = getwd()
load("~/SojournCode/nnet3ests.RData")
load("~/SojournCode/cent.1.RData")
load("~/SojournCode/scal.1.RData")
load("~/SojournCode/class.nnn.use.this.Rdata")

# load the R functions
source("~/SojournCode/sojourn.functions.R")

# load the nnet library
library(nnet)



library(data.table)
library(tree) 
library(randomForest)
library(plyr)
library(magrittr)
library(tidyverse)
library(devtools)
install_github('robmarcotte/MOCAfunctions')
install_github('robmarcotte/MOCAModelData', auth_token = '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') # need this auth token since it is a private repository (for now)
library(MOCAfunctions)
library(MOCAModelData)
library(data.table)
library(lubridate)
library(tools)
library(matrixStats)
library(randomForest)


pdf("~/showdata.pdf",width=8.5,height=11,onefile=T)

source("~/functions.R")

read.act.100Hz <- function(file.name.and.path)
{
  	head.data <- readLines(paste(file.name.and.path),n=10)
  	start.time <- head.data[3]
  	start.time <- (strsplit(start.time,split=" ")[[1]][3])
  	start.date <- head.data[4]
  	start.date <- (strsplit(start.date,split=" ")[[1]][3])
  	start.time <- as.POSIXlt(strptime(paste(start.date,start.time),"%m/%d/%Y %H:%M:%S"))
  	
	junk <- fread(file.name.and.path,skip=10,sep=",",header=T,showProgress=FALSE)
	n <- dim(junk)[1]

  	date.time <- start.time + (0:(n-1))/100
  	data <- data.frame(time=date.time,Axis1=junk[,1],Axis2=junk[,2],Axis3=junk[,3])
  	return(data)
}
read.act <- function(file.name.and.path)
{
  	head.data <- readLines(paste(file.name.and.path),n=10)
  	start.time <- head.data[3]
  	start.time <- (strsplit(start.time,split=" ")[[1]][3])
  	start.date <- head.data[4]
  	start.date <- (strsplit(start.date,split=" ")[[1]][3])
  	start.time <- as.POSIXlt(strptime(paste(start.date,start.time),"%m/%d/%Y %H:%M:%S"))
  	
	junk <- fread(file.name.and.path,skip=10,sep=",",header=T,showProgress=FALSE)
  	date.time <- as.POSIXlt(strptime(paste(junk$Date,junk$Time),"%m/%d/%Y %H:%M:%S"))
  	data <- data.frame(time=date.time,Axis1=junk$Axis1,Axis2=junk$Axis2,Axis3=junk$Axis3)
  	return(data)
}

folders <- 
c("CO_AIM1_Actigraph_1SEC_CSV_Data",
"CO_AIM1_Actigraph_RAW_CSA_Data",
"CO_AIM1_Merged_Chamber_RMR_Data")

chamber.files <- Sys.glob(paste0(path,folders[3],"/*"))

junk <- unlist(strsplit(chamber.files,"Chamber_RMR_Data/"))
n <- length(junk)
junk <- junk[seq(from=2,to=n,by=2)]
participants <- unlist(strsplit(junk,"\\."))[seq(from=1,to=n,by=2)]

counter <- 1

for (part.i in participants)
{
	# read the chamber data
	file.and.path <- paste0(path,folders[3],"/",part.i,".csv")
	chamber.data <- read.csv(file.and.path)
	chamber.data$datetime <- strptime(paste(chamber.data$datetime),"%Y-%m-%d %H:%M:%S")

	# make METs
	chamber.mets <-
		data.frame(time=rep(chamber.data$datetime,each=60),
			chamber.METs=rep(chamber.data$Chamber_V02_ml_kg_min/3.5,each=60))

#	chamber.mets <-
#		data.frame(time=rep(chamber.data$datetime,each=60),
#			chamber.METs=rep(chamber.data$Chamber_METs,each=60))
					
	chamber.mets$participant <- part.i

	start.time <- chamber.data$datetime[1]

	# the time formatting had problems for some participants  
	if (part.i==1009)
	{
		start.time <- strptime("2018-06-26 09:30:00",format="%Y-%m-%d %H:%M:%S")
		chamber.mets <- subset(chamber.mets,time>=start.time)
	}
	if (part.i==1067)
	{
		start.time <- strptime("2021-04-08 08:30:00",format="%Y-%m-%d %H:%M:%S")
		chamber.mets <- subset(chamber.mets,time>=start.time)
	}
	if (part.i==1069)
	{
		start.time <- strptime("2021-05-02 11:15:00",format="%Y-%m-%d %H:%M:%S")
		chamber.mets <- subset(chamber.mets,time>=start.time)
	}
	if (part.i==1075)
	{
		start.time <- strptime("2021-04-28 08:30:00",format="%Y-%m-%d %H:%M:%S")
		chamber.mets <- subset(chamber.mets,time>=start.time)
	}
	
	n.chamber <- dim(chamber.data)[1]
	end.time <- chamber.data$datetime[n.chamber]

	# read actigraph hip 1 sec
	file.and.path <- paste0(path,folders[1],"/","CO_AG_RH_",part.i,"1sec.csv")
	data <- read.act(file.and.path)
	hip.data.1sec <- subset(data,(data$time>=start.time)&(data$time<=end.time))
	hip.n.1sec <- dim(hip.data.1sec)[1]


	# read actigraph 1 sec wrist
	file.and.path <- paste0(path,folders[1],"/","CO_AG_RW_",part.i,"1sec.csv")
	data <- read.act(file.and.path)
	data.1sec <- subset(data,(data$time>=start.time)&(data$time<=end.time))
	n.1sec <- dim(data.1sec)[1]
	data.1sec$VM <- sqrt(data.1sec$Axis1^2+data.1sec$Axis2^2+data.1sec$Axis3^2)

	# read raw actigraph wrist
	file.and.path <- paste0(path,folders[2],"/","CO_AG_RW_",part.i,"RAW.csv")
	data <- read.act.100Hz(file.and.path)
	data.raw <- subset(data,(data$time>=start.time)&(data$time<=end.time))
	n.raw <- dim(data.raw)[1]
	names(data.raw)[2:4] <- c("AxisX","AxisY","AxisZ")
	data.raw$VM <- sqrt(data.raw$AxisX^2+data.raw$AxisY^2+data.raw$AxisZ^2)
	data.raw$VMcorrG <- data.raw$VM-1
	data.raw$VMcorrG[data.raw$VMcorrG<0] <- 0

	vm <- sqrt(hip.data.1sec$Axis1^2+hip.data.1sec$Axis2^2+hip.data.1sec$Axis3^2)	

	# apply sojourn 3x
	temp <- sojourn.3x(hip.data.1sec$Axis1,hip.data.1sec$Axis2,hip.data.1sec$Axis3,vm,short=30)$METs
	soj.est <- rep("sed",length(temp))
	soj.est[temp>1.5] <- "light"
	soj.est[temp>=3] <- "mvpa"
	
	# other methods
	row.est <- as.character(rowlands(data.raw))
	hil.est <- hildebrand(data.raw)	
	uma.est <- umass.wrist(data.raw)
	mon.est <- montoye(data.1sec$VM)
	fre.est <- freedson(hip.data.1sec$Axis1)
	
	data <- data.raw
	names(data)[1] <- "Timestamp"

	# rob marcotte's new method
	mar.est <- as.character(soj_g(data = data,export_format = "seconds",
  						freq = 100,step1_sd_threshold = 0.00375,
  						step2_nest_length = 5,step3_nest_length = 60,
  						step3_orig_soj_length_min = 180)$step3_estimate_intensity)
	mar.est[mar.est=="Sedentary"] <- "sed"
	mar.est[mar.est=="Light"] <- "light"
	mar.est[(mar.est=="Moderate")|(mar.est=="Vigorous")] <- "mvpa"
	
	chamber.mets$rowlands <- chamber.mets$hilde <- chamber.mets$umass <- chamber.mets$mont <- 
	chamber.mets$free <- chamber.mets$soj <- chamber.mets$mar <- NA

	
	chamber.mets$rowlands[1:length(row.est)] <- row.est
	chamber.mets$hilde[1:length(hil.est)] <- hil.est
	chamber.mets$umass[1:length(uma.est)] <- uma.est
	chamber.mets$mont[1:length(mon.est)] <- mon.est
	chamber.mets$free[1:length(fre.est)] <- fre.est
	chamber.mets$soj[1:length(soj.est)] <- soj.est
	chamber.mets$mar[1:length(mar.est)] <- mar.est
		
	chamber.mets$chamber.MET.lev <- "sed"	
	chamber.mets$chamber.MET.lev[chamber.mets$chamber.METs>1.5] <- "light"	
	chamber.mets$chamber.MET.lev[chamber.mets$chamber.METs>=3] <- "mvpa"	
	
	# add duration of MVPA and non-MVPA bouts
	chamber.mets$chamber.MET.lev <- factor(chamber.mets$chamber.MET.lev,levels=c("sed","light","mvpa"))

	y <- as.numeric(chamber.mets$chamber.MET.lev)
	y[y==2] <- 1
	# 1 means not mvpa, 3 means mvpa
	mmm <- length(y)
	one <- y[-mmm]
	two <- y[-1]

	# find transitions (up or down)
	trans <- (abs(one-two)>0) 	
	trans <- c(0,trans)
	if (sum(trans)>0)
	{
		trans.inds <- (1:mmm)[trans==1]
		trans.inds <- trans.inds-1
	
		durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]
		# put first duration in and make last trans go till end of file

		dd <- length(durations)
		tt <- length(trans.inds)
		durations[dd+1] <- mmm-trans.inds[tt]
		dd <- length(durations)
		durations.junk <- trans.inds[1]
		durations <- c(durations.junk,durations)
		chamber.mets$chamber.bout.durations <- rep(durations,durations)
	}
	if (sum(trans)==0)
		chamber.mets$chamber.bout.durations <- length(y)
	
	if (counter==1)
		summary.data <- chamber.mets
	if (counter>1)
		summary.data <- rbind(summary.data,chamber.mets)

	counter <- counter+1	

if (T)
{	
	if (is.na(chamber.mets$chamber.MET.lev[1])==F)
	{	
		if (T)
		{
		par(mfrow=c(3,1))
		plot(chamber.mets$chamber.METs,ylab="Chamber METs",type="l",main=part.i)
		plot(hip.data.1sec$Axis1*60,ylab="Hip Counts per Minute",type="l")
		plot(data.1sec$VM,ylab="Wrist VM Counts per Second",type="l")
		}		
		
		if (F)
		{
		par(mfrow=c(7,1))
		n.chamber <- length(chamber.mets$chamber.MET.lev)
		
		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$chamber.MET.lev=="sed")+
							1*(chamber.mets$chamber.MET.lev=="light")+
								2*(chamber.mets$chamber.MET.lev=="mvpa"))

		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$mont=="sed")+
							1*(chamber.mets$mont=="light")+
								2*(chamber.mets$mont=="mvpa"),col="blue")
		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$umass=="sed")+
							1*(chamber.mets$umass=="light")+
								2*(chamber.mets$umass=="mvpa"),col="red")
		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$hilde=="sed")+
							1*(chamber.mets$hilde=="light")+
								2*(chamber.mets$hilde=="mvpa"),col="green")
		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$rowlands=="sed")+
							1*(chamber.mets$rowlands=="light")+
								2*(chamber.mets$rowlands=="mvpa"),col="cyan")
		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$free=="sed")+
							1*(chamber.mets$free=="light")+
								2*(chamber.mets$free=="mvpa"),col="magenta")
		plot(1:n.chamber,1:n.chamber,type="n",xlab="min",ylab="MET Level",
			main=paste0("Chamber, part=",part.i),ylim=c(0,2))
		lines(1:n.chamber,0*(chamber.mets$mar=="sed")+
							1*(chamber.mets$mar=="light")+
								2*(chamber.mets$mar=="mvpa"),col="purple")
		
	}
	
}	
}
}
dev.off()


summary.data$mar <- factor(summary.data$mar,levels=c("sed","light","mvpa"))
summary.data$soj <- factor(summary.data$soj,levels=c("sed","light","mvpa"))
summary.data$free <- factor(summary.data$free,levels=c("sed","light","mvpa"))
summary.data$hilde <- factor(summary.data$hilde,levels=c("sed","light","mvpa"))
summary.data$mont <- factor(summary.data$mont,levels=c("sed","light","mvpa"))
summary.data$rowlands <- factor(summary.data$rowlands,levels=c("sed","light","mvpa"))
summary.data$umass <- factor(summary.data$umass,levels=c("sed","light","mvpa"))
summary.data$chamber.MET.lev <- factor(summary.data$chamber.MET.lev,levels=c("sed","light","mvpa"))

overall.perc <- rbind( 
	table(summary.data$soj)/sum(table(summary.data$soj)),
	table(summary.data$free)/sum(table(summary.data$free)),
	table(summary.data$mar)/sum(table(summary.data$mar)),
	table(summary.data$hilde)/sum(table(summary.data$hilde)),
	table(summary.data$mont)/sum(table(summary.data$mont)),
	table(summary.data$rowlands)/sum(table(summary.data$rowlands)),
		table(summary.data$umass)/sum(table(summary.data$umass)),
			table(summary.data$chamber.MET.lev)/sum(table(summary.data$chamber.MET.lev)))
	
overall.perc <- as.data.frame(overall.perc)
names(overall.perc) <- c("sed","light","mvpa")
rownames(overall.perc) <- c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","Umass","Chamber")

overall.bias.table <- overall.perc[1:7,]
overall.bias.table <- t(t(overall.bias.table) - unlist(overall.perc[8,]))

se.bias.table <- rbind(apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(soj=="sed",na.rm=T),mean(soj=="light",na.rm=T),mean(soj=="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21),
 	apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(free=="sed",na.rm=T),mean(free=="light",na.rm=T),mean(free=="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21),
apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(mar=="sed",na.rm=T),mean(mar=="light",na.rm=T),mean(mar=="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21),
apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(hilde=="sed",na.rm=T),mean(hilde=="light",na.rm=T),mean(hilde=="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21),
apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(mont=="sed",na.rm=T),mean(mont=="light",na.rm=T),mean(mont=="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21),
apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(rowlands =="sed",na.rm=T),mean(rowlands =="light",na.rm=T),mean(rowlands =="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21),
apply(ddply(summary.data,
 	.(participant),summarize,
 	mean(umass=="sed",na.rm=T),mean(umass =="light",na.rm=T),mean(umass =="mvpa",na.rm=T))[,-1],2,sd)/sqrt(21))


overall.bias.table <- overall.bias.table*100
se.bias.table <- se.bias.table*100
lower <- overall.bias.table - 1.96*se.bias.table
upper <- overall.bias.table + 1.96*se.bias.table


delta <- .1
plot(0:8,0:8,type="n",axes=F,ylab="Overall Bias (% Estimated in Category - Chamber Criteria) and 95% CI",xlab="Method",
	xlim=c(.75,7.75),
	ylim=range(rbind(lower,upper)))
for (i in 1:7)
{
	points(i-delta,overall.bias.table[i,1],pch=1)
	points(i,overall.bias.table[i,2],pch=5)
	points(i+delta,overall.bias.table[i,3],pch=16)
	
	lines(c(i-delta,i-delta),c(lower[i,1],upper[i,1]))
	lines(c(i,i),c(lower[i,2],upper[i,2]))
	lines(c(i+delta,i+delta),c(lower[i,3],upper[i,3]))
}
axis(2)
abline(h=0,lty=3)
axis(1,at=1:7,row.names(overall.bias.table),cex.axis=.75)
legend("topright",legend=c("sed","light","mvpa"),pch=c(1,5,16))


sec.sec.perc.agree <- data.frame(methods=c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","Umass"),
								agree=c(mean(summary.data$soj==summary.data$chamber.MET.lev,na.rm=T),
										mean(summary.data$free==summary.data$chamber.MET.lev,na.rm=T),
										mean(summary.data$mar==summary.data$chamber.MET.lev,na.rm=T),
										mean(summary.data$hilde==summary.data$chamber.MET.lev,na.rm=T),
										mean(summary.data$mont==summary.data$chamber.MET.lev,na.rm=T),
										mean(summary.data$rowlands==summary.data$chamber.MET.lev,na.rm=T),
										mean(summary.data$umass==summary.data$chamber.MET.lev,na.rm=T)))

se.agree.table <- apply(cbind(
ddply(summary.data,.(participant),summarize,sd(soj==chamber.MET.lev,na.rm=T))[,2]/sqrt(21),
ddply(summary.data,.(participant),summarize,sd(free==chamber.MET.lev,na.rm=T))[,2]/sqrt(21),
ddply(summary.data,.(participant),summarize,sd(mar==chamber.MET.lev,na.rm=T))[,2]/sqrt(21),
ddply(summary.data,.(participant),summarize,sd(hilde==chamber.MET.lev,na.rm=T))[,2]/sqrt(21),
ddply(summary.data,.(participant),summarize,sd(mont==chamber.MET.lev,na.rm=T))[,2]/sqrt(21),
ddply(summary.data,.(participant),summarize,sd(rowlands==chamber.MET.lev,na.rm=T))[,2]/sqrt(21),
ddply(summary.data,.(participant),summarize,sd(umass==chamber.MET.lev,na.rm=T))[,2]/sqrt(21)),2,mean)



lower <- sec.sec.perc.agree[,2] - 1.96*se.agree.table
upper <- sec.sec.perc.agree[,2] + 1.96*se.agree.table
upper[upper>1] <- 1

sec.sec.perc.agree[,2] <- sec.sec.perc.agree[,2]*100
lower <- lower*100
upper <- upper*100



plot(0:8,0:8,type="n",axes=F,ylab="Second by Second Percent Agreement and 95% CI",xlab="Method",
	xlim=c(.75,7.75),
	ylim=c(0,100))
for (i in 1:7)
{
	points(i,sec.sec.perc.agree[i,2],pch=16)
	lines(c(i,i),c(lower[i],upper[i]))
}
axis(2)
axis(1,at=1:7,sec.sec.perc.agree[,1],cex.axis=.75)




soj.conf.mat <- table(summary.data$soj,summary.data$chamber.MET.lev)/sum(table(summary.data$soj,summary.data$chamber.MET.lev))
free.conf.mat <- table(summary.data$free,summary.data$chamber.MET.lev)/sum(table(summary.data$free,summary.data$chamber.MET.lev))
mar.conf.mat <- table(summary.data$mar,summary.data$chamber.MET.lev)/sum(table(summary.data$mar,summary.data$chamber.MET.lev))
hild.conf.mat <- table(summary.data$hilde,summary.data$chamber.MET.lev)/sum(table(summary.data$hilde,summary.data$chamber.MET.lev))
mont.conf.mat <- table(summary.data$mont,summary.data$chamber.MET.lev)/sum(table(summary.data$mont,summary.data$chamber.MET.lev))
rowlands.conf.mat <- table(summary.data$rowlands,summary.data$chamber.MET.lev)/sum(table(summary.data$rowlands,summary.data$chamber.MET.lev))
umass.conf.mat <- table(summary.data$umass,summary.data$chamber.MET.lev)/sum(table(summary.data$umass,summary.data$chamber.MET.lev))



# when there's a bout MVPA >5' according to chamber, what % of time do methods say it's MVPA?
# when there's a bout of non-MVPA >5' according to chamber, what % of time do methods say it's not MVPA?

# find hip and wrist counts>0 bouts of various durations. 
# use hip and wrist to see if those bouts are activity?
# What % of time does chamber pick up activity?


# bouts >5' according to chamber
gt.5.min <- subset(summary.data,(chamber.MET.lev=="mvpa")&(chamber.bout.durations>=300))

# fraction of time methods say it's MVPA
sensitivity <- data.frame(method=c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","UMass","Chamber"),
							sense=c(mean(gt.5.min$soj=="mvpa",na.rm=T),
									mean(gt.5.min$free=="mvpa",na.rm=T),
									mean(gt.5.min$mar=="mvpa",na.rm=T),
									mean(gt.5.min$hilde=="mvpa",na.rm=T),
									mean(gt.5.min$mont=="mvpa",na.rm=T),
									mean(gt.5.min$rowlands=="mvpa",na.rm=T),
									mean(gt.5.min$umass=="mvpa",na.rm=T),
									mean(gt.5.min$chamber.MET.lev=="mvpa",na.rm=T)))

se.sense.table <- c(
sd(ddply(gt.5.min,.(participant),summarize,mean(soj=="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(gt.5.min,.(participant),summarize,mean(free=="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(gt.5.min,.(participant),summarize,mean(mar=="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(gt.5.min,.(participant),summarize,mean(hilde=="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(gt.5.min,.(participant),summarize,mean(mont=="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(gt.5.min,.(participant),summarize,mean(rowlands=="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(gt.5.min,.(participant),summarize,mean(umass=="mvpa",na.rm=T))[,2])/sqrt(21))

lower <- sensitivity[,2] - 1.96*se.sense.table
upper <- sensitivity[,2] + 1.96*se.sense.table
upper[upper>1] <- 1

sensitivity[,2] <- sensitivity[,2]*100
lower <- lower*100
upper <- upper*100



plot(0:8,0:8,type="n",axes=F,ylab="% agreement of 5'+ bouts",xlab="Method",
	xlim=c(.75,7.75),
	ylim=c(0,100))
for (i in 1:7)
{
	points(i, sensitivity[i,2],pch=16)
	lines(c(i,i),c(lower[i],upper[i]))
}
axis(2)
axis(1,at=1:7, sensitivity[1:7,1],cex.axis=.75)



# not bouts >5' according to chamber
nnn <- dim(summary.data)[1]
inds <- (1:nnn)[(summary.data$chamber.MET.lev=="mvpa")&(summary.data$chamber.bout.durations>=300)]

not.gt.5.min <- summary.data[-inds,]

# fraction of time methods say it's MVPA
selectivity <- data.frame(method=c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","UMass","Chamber"),
							select=c(mean(not.gt.5.min$soj!="mvpa",na.rm=T),
									mean(not.gt.5.min$free!="mvpa",na.rm=T),
									mean(not.gt.5.min$mar!="mvpa",na.rm=T),
									mean(not.gt.5.min$hilde!="mvpa",na.rm=T),
									mean(not.gt.5.min$mont!="mvpa",na.rm=T),
									mean(not.gt.5.min$rowlands!="mvpa",na.rm=T),
									mean(not.gt.5.min$umass!="mvpa",na.rm=T),
									mean(not.gt.5.min$chamber.MET.lev!="mvpa",na.rm=T)))

se.select.table <- c(
sd(ddply(not.gt.5.min,.(participant),summarize,mean(soj!="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(not.gt.5.min,.(participant),summarize,mean(free!="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(not.gt.5.min,.(participant),summarize,mean(mar!="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(not.gt.5.min,.(participant),summarize,mean(hilde!="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(not.gt.5.min,.(participant),summarize,mean(mont!="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(not.gt.5.min,.(participant),summarize,mean(rowlands!="mvpa",na.rm=T))[,2])/sqrt(21),
sd(ddply(not.gt.5.min,.(participant),summarize,mean(umass!="mvpa",na.rm=T))[,2])/sqrt(21))

lower <- selectivity[,2] - 1.96*se.select.table
upper <- selectivity[,2] + 1.96*se.select.table
upper[upper>1] <- 1

selectivity[,2] <- selectivity[,2]*100
lower <- lower*100
upper <- upper*100



plot(0:8,0:8,type="n",axes=F,ylab="% agreement of non 5'+ bouts",xlab="Method",
	xlim=c(.75,7.75),
	ylim=c(0,100))
for (i in 1:7)
{
	points(i, selectivity[i,2],pch=16)
	lines(c(i,i),c(lower[i],upper[i]))
}
axis(2)
axis(1,at=1:7, selectivity[1:7,1],cex.axis=.75)


# use 1001 to show chamber bout with accelerometer bout
part.i <- "1001"

junk <- subset(summary.data,participant==part.i)
n <- dim(junk)[1]

start <- strptime("2018-03-16 10:56:17",format="%Y-%m-%d %H:%M:%S")
end <- strptime("2018-03-16 11:46:17",format="%Y-%m-%d %H:%M:%S")



file.and.path <- paste0(path,folders[3],"/",part.i,".csv")
chamber.data <- read.csv(file.and.path)
chamber.data$datetime <- strptime(paste(chamber.data$datetime),"%Y-%m-%d %H:%M:%S")
start.time <- chamber.data$datetime[1]

# read actigraph 1 sec
file.and.path <- paste0(path,folders[1],"/","CO_AG_RW_",part.i,"1sec.csv")
data <- read.act(file.and.path)
data.1sec <- subset(data,(data$time>=start.time)&(data$time<=end.time))
n.1sec <- dim(data.1sec)[1]
data.1sec$VM <- sqrt(data.1sec$Axis1^2+data.1sec$Axis2^2+data.1sec$Axis3^2)

# read actigraph hip 1 sec
#file.and.path <- paste0(path,folders[1],"/","CO_AG_RH_",part.i,"1sec.csv")
#data <- read.act(file.and.path)
#hip.data.1sec <- subset(data,(data$time>=start.time)&(data$time<=end.time))
#hip.n.1sec <- dim(hip.data.1sec)[1]

inds <- (1:n)[(junk$time>=start)&(junk$time<=end)]
par(mar = c(5, 4, 4, 4) + 0.3)
plot((1:n)[inds],junk$chamber.METs[inds],type="l",lwd=3,
xlab="Seconds",ylab="METs (chamber)")

inds <- (1:n.1sec)[(data.1sec$time>=start)&(data.1sec$time<=end)]
vm <- data.1sec$VM[inds]
par(new = TRUE)

plot(inds,vm,type="l",axes=F,bty="n",xlab="",ylab="",col="grey")
axis(side=4, at = pretty(range(vm)))
mtext("Wrist Actigraph Counts Vector Magnitude per Second", side=4, line=3)

legend("topleft",c("METs","Vector Magnitude"),lwd=c(3,1),col=c("black","grey"))



# use 1002 where chamber bout occurs but accelerometer doesn't

part.i <- "1002"
junk <- subset(summary.data,participant==part.i)
n <- dim(junk)[1]

start <- strptime("2018-03-29 12:45:00",format="%Y-%m-%d %H:%M:%S")
end <- strptime("2018-03-29 13:30:00",format="%Y-%m-%d %H:%M:%S")



file.and.path <- paste0(path,folders[3],"/",part.i,".csv")
chamber.data <- read.csv(file.and.path)
chamber.data$datetime <- strptime(paste(chamber.data$datetime),"%Y-%m-%d %H:%M:%S")
start.time <- chamber.data$datetime[1]


# read actigraph hip 1 sec
#file.and.path <- paste0(path,folders[1],"/","CO_AG_RH_",part.i,"1sec.csv")
#data <- read.act(file.and.path)
#hip.data.1sec <- subset(data,(data$time>=start.time)&(data$time<=end.time))
#hip.n.1sec <- dim(hip.data.1sec)[1]

# read actigraph 1 sec
file.and.path <- paste0(path,folders[1],"/","CO_AG_RW_",part.i,"1sec.csv")
data <- read.act(file.and.path)
data.1sec <- subset(data,(data$time>=start.time)&(data$time<=end.time))
n.1sec <- dim(data.1sec)[1]
data.1sec$VM <- sqrt(data.1sec$Axis1^2+data.1sec$Axis2^2+data.1sec$Axis3^2)


inds <- (1:n)[(junk$time>=start)&(junk$time<=end)]
par(mar = c(5, 4, 4, 4) + 0.3)
plot((1:n)[inds],junk$chamber.METs[inds],type="l",lwd=3,
xlab="Seconds",ylab="METs (chamber)")

inds <- (1:n.1sec)[(data.1sec$time>=start)&(data.1sec$time<=end)]
vm <- data.1sec$VM[inds] 
par(new = TRUE)

plot(inds,vm,type="l",axes=F,bty="n",xlab="",ylab="",col="grey")
axis(side=4, at = pretty(range(vm)))
mtext("Wrist Actigraph Counts Vector Magnitude per Second", side=4, line=3)

legend("topleft",c("METs","Vector Magnitude"),lwd=c(3,1),col=c("black","grey"))















mvpa.or.not.summary.data <- summary.data
n <- dim(mvpa.or.not.summary.data)[1]

mvpa.or.not.summary.data$soj <- as.character(mvpa.or.not.summary.data$soj)
inds <- (1:n)[(mvpa.or.not.summary.data$soj=="sed")|(mvpa.or.not.summary.data$soj=="light")]
mvpa.or.not.summary.data$soj[inds] <- "not mvpa"

mvpa.or.not.summary.data$free <- as.character(mvpa.or.not.summary.data$free)
inds <- (1:n)[(mvpa.or.not.summary.data$free=="sed")|(mvpa.or.not.summary.data$free=="light")]
mvpa.or.not.summary.data$free[inds] <- "not mvpa"

mvpa.or.not.summary.data$mar <- as.character(mvpa.or.not.summary.data$mar)
inds <- (1:n)[(mvpa.or.not.summary.data$mar=="sed")|(mvpa.or.not.summary.data$mar=="light")]
mvpa.or.not.summary.data$mar[inds] <- "not mvpa"

mvpa.or.not.summary.data$hilde <- as.character(mvpa.or.not.summary.data$hilde)
inds <- (1:n)[(mvpa.or.not.summary.data$hilde =="sed")|(mvpa.or.not.summary.data$hilde =="light")]
mvpa.or.not.summary.data$hilde[inds] <- "not mvpa"

mvpa.or.not.summary.data$mont <- as.character(mvpa.or.not.summary.data$mont)
inds <- (1:n)[(mvpa.or.not.summary.data$mont =="sed")|(mvpa.or.not.summary.data$mont =="light")]
mvpa.or.not.summary.data$mont[inds] <- "not mvpa"

mvpa.or.not.summary.data$rowlands <- as.character(mvpa.or.not.summary.data$rowlands)
inds <- (1:n)[(mvpa.or.not.summary.data$rowlands =="sed")|(mvpa.or.not.summary.data$rowlands =="light")]
mvpa.or.not.summary.data$rowlands[inds] <- "not mvpa"

mvpa.or.not.summary.data$umass <- as.character(mvpa.or.not.summary.data$umass)
inds <- (1:n)[(mvpa.or.not.summary.data$umass =="sed")|(mvpa.or.not.summary.data$umass =="light")]
mvpa.or.not.summary.data$umass[inds] <- "not mvpa"

mvpa.or.not.summary.data$chamber.MET.lev <- as.character(mvpa.or.not.summary.data$chamber.MET.lev)
inds <- (1:n)[(mvpa.or.not.summary.data$chamber.MET.lev=="sed")|(mvpa.or.not.summary.data$chamber.MET.lev=="light")]
mvpa.or.not.summary.data$chamber.MET.lev[inds] <- "not mvpa"

overall.perc <- rbind( 
	table(mvpa.or.not.summary.data$soj)/sum(table(mvpa.or.not.summary.data$soj)),
	table(mvpa.or.not.summary.data$free)/sum(table(mvpa.or.not.summary.data$free)),
	table(mvpa.or.not.summary.data$mar)/sum(table(mvpa.or.not.summary.data$mar)),
	table(mvpa.or.not.summary.data$hilde)/sum(table(mvpa.or.not.summary.data$hilde)),
	table(mvpa.or.not.summary.data$mont)/sum(table(mvpa.or.not.summary.data$mont)),
	table(mvpa.or.not.summary.data$rowlands)/sum(table(mvpa.or.not.summary.data$rowlands)),
		table(mvpa.or.not.summary.data$umass)/sum(table(mvpa.or.not.summary.data$umass)),
			table(mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$chamber.MET.lev)))
	
overall.perc <- as.data.frame(overall.perc)
names(overall.perc) <- c("mvpa","not mvpa")
rownames(overall.perc) <- c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","Umass","Chamber")

sec.sec.perc.agree <- data.frame(methods=c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","Umass"),
								agree=c(mean(mvpa.or.not.summary.data$soj==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T),
										mean(mvpa.or.not.summary.data$free==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T),
										mean(mvpa.or.not.summary.data$mar==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T),
										mean(mvpa.or.not.summary.data$hilde==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T),
										mean(mvpa.or.not.summary.data$mont==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T),
										mean(mvpa.or.not.summary.data$rowlands==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T),
										mean(mvpa.or.not.summary.data$umass==mvpa.or.not.summary.data$chamber.MET.lev,na.rm=T)))




# bouts >5' according to chamber
gt.5.min <- subset(mvpa.or.not.summary.data,(chamber.MET.lev=="mvpa")&(chamber.bout.durations>=300))

# fraction of time methods say it's MVPA
sensitivity <- data.frame(method=c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","UMass","Chamber"),
							sense=c(mean(gt.5.min$soj=="mvpa",na.rm=T),
									mean(gt.5.min$free=="mvpa",na.rm=T),
									mean(gt.5.min$mar=="mvpa",na.rm=T),
									mean(gt.5.min$hilde=="mvpa",na.rm=T),
									mean(gt.5.min$mont=="mvpa",na.rm=T),
									mean(gt.5.min$rowlands=="mvpa",na.rm=T),
									mean(gt.5.min$umass=="mvpa",na.rm=T),
									mean(gt.5.min$chamber.MET.lev=="mvpa",na.rm=T)))

# not bouts >5' according to chamber
nnn <- dim(mvpa.or.not.summary.data)[1]
inds <- (1:nnn)[(mvpa.or.not.summary.data$chamber.MET.lev=="mvpa")&(mvpa.or.not.summary.data$chamber.bout.durations>=300)]

not.gt.5.min <- mvpa.or.not.summary.data[-inds,]

# fraction of time methods say it's MVPA
selectivity <- data.frame(method=c("Sojourn","Freedson","Marcotte","Hildebrand","Montoye","Rowlands","UMass","Chamber"),
							select=c(mean(not.gt.5.min$soj!="mvpa",na.rm=T),
									mean(not.gt.5.min$free!="mvpa",na.rm=T),
									mean(not.gt.5.min$mar!="mvpa",na.rm=T),
									mean(not.gt.5.min$hilde!="mvpa",na.rm=T),
									mean(not.gt.5.min$mont!="mvpa",na.rm=T),
									mean(not.gt.5.min$rowlands!="mvpa",na.rm=T),
									mean(not.gt.5.min$umass!="mvpa",na.rm=T),
									mean(not.gt.5.min$chamber.MET.lev!="mvpa",na.rm=T)))


soj.conf.mat <- table(mvpa.or.not.summary.data$soj,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$soj,mvpa.or.not.summary.data$chamber.MET.lev))
free.conf.mat <- table(mvpa.or.not.summary.data$free,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$free,mvpa.or.not.summary.data$chamber.MET.lev))
mar.conf.mat <- table(mvpa.or.not.summary.data$mar,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$mar,mvpa.or.not.summary.data$chamber.MET.lev))
hild.conf.mat <- table(mvpa.or.not.summary.data$hilde,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$hilde,mvpa.or.not.summary.data$chamber.MET.lev))
mont.conf.mat <- table(mvpa.or.not.summary.data$mont,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$mont,mvpa.or.not.summary.data$chamber.MET.lev))
rowlands.conf.mat <- table(mvpa.or.not.summary.data$rowlands,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$rowlands,mvpa.or.not.summary.data$chamber.MET.lev))
umass.conf.mat <- table(mvpa.or.not.summary.data$umass,mvpa.or.not.summary.data$chamber.MET.lev)/sum(table(mvpa.or.not.summary.data$umass,mvpa.or.not.summary.data$chamber.MET.lev))

# find some examples - figures


