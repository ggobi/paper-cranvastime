########## Figure 1,2 ##########################################################
devtools::load_all('~/Software/cranvas')
library(nlme)
qRem <- qdata(Remifentanil[complete.cases(Remifentanil) & Remifentanil$ID==1 & Remifentanil$Time<=41,])
qtime(Time, conc, qRem, series.stats=FALSE, ylab='Value')

Remi <- Remifentanil[complete.cases(Remifentanil) & Remifentanil$Time<=41,]
Remi$ID <- factor(Remi$ID)
qRemi <- qdata(Remi)
qtime(Time, conc, qRemi, vdiv=ID, ylab='Value')

########## Figure 6,7,12 #######################################################
nasa21 = subset(nasa, Gridx %in% 19:21 & Gridy == 21)
nasa21$Year = factor(nasa21$Year)
nasa21$Gridx = factor(as.integer(factor(nasa21$Gridx)))
colnames(nasa21)[c(1,6,9,14)]=c('Individual','Time','A','B')
qnasa = qdata(nasa21)
qtime(Time,c(A,B),qnasa,vdiv=Individual,shift=c(1,12))


########## Figure 1,8 ##########################################################
nasa2221 = subset(nasa, Gridx==22 & Gridy == 21)
nasa2221$Year = factor(as.integer(factor(nasa2221$Year)))
colnames(nasa2221)[c(6,7,9)]=c('Time','Period','Value')
nasa2221$Value = (nasa2221$Value - min(nasa2221$Value))/(diff(range(nasa2221$Value)))
qnasa2221 = qdata(nasa2221)
qtime(Time,Value,qnasa2221,shift=c(1,12),series.stats=FALSE)
qtime(Time,Value,qnasa2221,shift=c(1,12),vdiv=Period)


########## Figure 9 ############################################################
qlynx <- qdata(data.frame(Time=1:114, lynx))
qtime(Time, lynx, qlynx, shift=1:13, series.stats=FALSE)


########## Figure 14 ###########################################################
flu.data <- read.table("http://www.google.org/flutrends/us/data.txt", skip=11, sep=",", header=TRUE)
flu.data <- flu.data[, c(1, 3:53)]
library(reshape)
flu.melt <- melt(flu.data, id.vars="Date")
flu.melt$Date <- as.Date(flu.melt$Date)
colnames(flu.melt)[2] <- "State"
colnames(flu.melt)[3] <- "FluSearches"
flu.melt$days <- as.vector(difftime(flu.melt$Date,as.Date('2002-12-31')))
flu2014 <- subset(flu.melt, days>3980 & days<4100)
ord <- names(sort(tapply(flu2014$FluSearches,flu2014$State,function(x)which(x>(max(x)/5*3))[1])))
flu2014$State <- factor(flu2014$State,levels=ord)
qflu <- qdata(flu2014)
qtime(days, FluSearches, data=qflu, vdiv="State",shift=c(1,7,28,35,91))

########## Figure 15 ###########################################################

dat = data.frame(x=1:8,y=c(21,24,30,0,12,8,8,11))

par(mar=c(2,2.5,1,1))
plot(dat,type='n',ylim=c(-2,32),yaxt='n',xaxt='n',ylab='',frame=FALSE)
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,4,4,3,NA,
          4,4,5,5,4,NA,
          5,5,6,6,5,NA,
          6,6,7,7,6,NA,
          7,7,8,8,7,NA),
        c(21,0,0,24,21,NA,
          24,0,0,30,24,NA,
          30,0,0,0,30,NA,
          0,0,0,12,0,NA,
          12,0,0,8,12,NA,
          8,0,0,8,8,NA,
          8,0,0,11,8,NA),
        col='grey90')
lines(dat$x,dat$y,type='o')
abline(a=0,b=0)
abline(a=10,b=0,lty=3)
abline(a=20,b=0,lty=3)
abline(a=30,b=0,lty=3)
points(dat$x,dat$y,pch=19,cex=1.8,col=1)

par(mfrow=c(3,1),mar=c(2,4,1,2))
plot(dat,type='n',ylim=c(20,30.2),frame=FALSE,yaxt='n',xaxt='n',xlab='',ylab='')
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,3.333,3.333,3,NA),
        c(21,20,20,24,21,NA,
          24,20,20,30,24,NA,
          30,20,20,20,30,NA),
        col='grey90')
#lines(dat$x,dat$y,type='o')
abline(a=20,b=0,lty=3)
abline(a=30,b=0,lty=3)
points(dat$x,dat$y,pch=19,cex=1.8,col=1)
points(c(1,2,3,3.333),
       c(20,20,20,20),
       pch=19,cex=1.8,col=2)

plot(dat,type='n',ylim=c(10,20.2),frame=FALSE,yaxt='n',xaxt='n',xlab='',ylab='')
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,3.667,3.333,3,NA,
          4.833,4.833,5,5,4.833,NA,
          5,5,5.5,5.5,5,NA,
          7.667,7.667,8,8,7.667,NA),
        c(20,10,10,20,20,NA,
          20,10,10,20,20,NA,
          20,10,10,20,20,NA,
          10,10,10,12,10,NA,
          12,10,10,10,12,NA,
          10,10,10,11,10,NA),
        col='grey90')
#lines(dat$x,dat$y,type='o')
abline(a=10,b=0,lty=3)
abline(a=20,b=0,lty=3)
points(dat$x,dat$y,pch=19,cex=1.8,col=1)
points(c(1,1,2,2,3,3,3.333,3.667,4.833,5,5.5,7.667,8),
       c(20,10,20,10,20,10,20,10,10,10,10,10,10),
       pch=19,cex=1.8,col=2)

plot(dat,type='n',ylim=c(0,10.1),frame=FALSE,yaxt='n',xaxt='n',ylab='')
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,4,3.667,3,NA,
          4,5,5,4.833,4,NA,
          5,5,6,6,5.5,5,NA,
          6,6,7,7,6,NA,
          7,7,8,8,7.667,7,NA),
        c(10,0,0,10,10,NA,
          10,0,0,10,10,NA,
          10,0,0,10,10,NA,
          0,0,10,10,0,NA,
          10,0,0,8,10,10,NA,
          8,0,0,8,8,NA,
          8,0,0,10,10,8,NA),
        col='grey90')
#lines(dat$x,dat$y,type='o')
abline(a=0,b=0,lty=3)
abline(a=10,b=0,lty=3)
points(dat$x,dat$y,pch=19,cex=1.8,col=1)
points(c(1,1,2,2,3,3,3.667,4.833,5,5,5.5,6,7,7.667,8,8),
       c(10,0,10,0,10,0,10,10,10,0,10,0,0,10,10,0),
       pch=19,cex=1.8,col=2)


par(mar=c(2,2.5,1,1))
plot(dat,type='n',ylim=c(0.3,9.7),yaxt='n',xaxt='n',ylab='',frame=FALSE)
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,4,3.667,3,NA,
          4,5,5,4.833,4,NA,
          5,5,6,6,5.5,5,NA,
          6,6,7,7,6,NA,
          7,7,8,8,7.667,7,NA),
        c(10,0,0,10,10,NA,
          10,0,0,10,10,NA,
          10,0,0,10,10,NA,
          0,0,10,10,0,NA,
          10,0,0,8,10,10,NA,
          8,0,0,8,8,NA,
          8,0,0,10,10,8,NA),
        col='grey90')
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,3.667,3.333,3,NA,
          4.833,4.833,5,5,4.833,NA,
          5,5,5.5,5.5,5,NA,
          7.667,7.667,8,8,7.667,NA),
        c(10,0,0,10,10,NA,
          10,0,0,10,10,NA,
          10,0,0,10,10,NA,
          0,0,0,2,0,NA,
          2,0,0,0,2,NA,
          0,0,0,1,0,NA),
        col='grey70')
polygon(c(1,1,2,2,1,NA,
          2,2,3,3,2,NA,
          3,3,3.333,3.333,3,NA),
        c(1,0,0,4,1,NA,
          4,0,0,10,4,NA,
          10,0,0,0,10,NA),
        col='grey50')
points(dat$x,dat$y)
points(dat$x,dat$y-10)
points(dat$x,dat$y-20)
abline(a=0,b=0,lty=3)
abline(a=10,b=0,lty=3)
