devtools::load_all('~/Software/cranvas')


########## Figure 1 ############################################################
library(ggplot2)
nasa2221 = subset(nasa, Gridx == 22 & Gridy == 21)
nasa2221$ts = (nasa2221$ts-min(nasa2221$ts))/(diff(range(nasa2221$ts)))
pdf("pipeline-01-regular.pdf",width=5,height=3)
qplot(TimeIndx, ts, data=nasa2221, geom='line', xlab='Time', ylab='Value') +
  geom_point(size=I(2)) +
  geom_rug(sides="b",size=I(0.2)) #+
  #theme(axis.title = element_text(size = rel(1.5)),
  #      axis.text = element_text(size = rel(1.2)))
dev.off()

library(nlme)
Rem = Remifentanil[complete.cases(Remifentanil) & Remifentanil$ID==1 & Remifentanil$Time<=41,]
# qRem <- qdata(Rem)
# qtime(Time, conc, qRem, series.stats=FALSE, ylab='Value')
pdf("pipeline-01-irregular.pdf",width=5,height=3)
qplot(Time, conc, data=Rem, geom='line',ylab='Value') +
  geom_point(size=I(2)) +
  geom_rug(sides="b",size=I(0.2)) #+
  #theme(axis.title = element_text(size = rel(1.5)),
  #      axis.text = element_text(size = rel(1.2)))
dev.off()


########## Figure 2 ############################################################
#Remi <- Remifentanil[complete.cases(Remifentanil) & Remifentanil$Time<=41,]
#Remi$ID <- factor(Remi$ID)
#qRemi <- qdata(Remi)
#qtime(Time, conc, qRemi, vdiv=ID, ylab='Value')

pig = pigs[,c(1:3,7:8,10:11)]
for (i in 4:7) pig[,i] = (pig[,i]-min(pig[,i]))/diff(range(pig[,i]))
names(pig)[4:7]=paste0("V",1:4)
pig = reshape2::melt(pig,1:3)
pig$variable = factor(as.character(pig$variable),levels=paste0('V',4:1))

pdf("pipeline-02-linegraph.pdf",width=5,height=3)
qplot(TIME, value, data=pig, geom='line', group=variable, color=variable, size=I(1), xlab='Time', ylab='Value') + theme(legend.position='none')
dev.off()

pdf("pipeline-02-multilines.pdf",width=5,height=3)
qplot(TIME, value, data=pig, geom='line', group=variable, color=variable, size=I(1), xlab='Time', ylab='Value',facets=variable~.) + theme(legend.position='none') + scale_y_continuous(breaks = c(0,0.5,1))
dev.off()

pdf("pipeline-02-smallmultiples.pdf",width=5,height=3)
qplot(TIME, value, data=pig, geom='line', group=variable, color=variable, xlab='Time', ylab='Value',facets=variable~.) + geom_area(aes(fill=variable)) + theme(legend.position='none') + scale_y_continuous(breaks = c(0,0.5,1))
dev.off()

pdf("pipeline-02-stacked.pdf",width=5,height=3)
qplot(TIME, value, data=pig, geom=c('area'), group=variable, color=variable, fill=variable, xlab='Time', ylab='Value') + theme(legend.position='none')
dev.off()

pig = pig[,-(2:3)]
pig$sign = sign(pig$value-0.5)
pig$mirrorUP = pmax(pig$value-0.5,0)
pig$mirrorDOWN = pmax(0.5-pig$value,0)
idx0=setdiff(which(diff(pig$sign)!=0),1:4*48)
for (i in 1:length(idx0)){
  wt = (0.5-pig$value[idx0[i]])/diff(pig$value[0:1+idx0[i]])
  pig=rbind(pig,data.frame(TIME=pig$TIME[idx0[i]]+wt,variable=pig$variable[idx0[i]],value=0.5,sign=0,mirrorUP=0,mirrorDOWN=0))
}
pig=pig[order(pig$variable,pig$TIME),]
pig$levelUP = pig$mirrorUP %/% 0.2
pig$valueUP0 = pmin(pig$mirrorUP,0.2)
pig$valueUP1 = pmin(pmax(pig$mirrorUP,0.2),0.4)-0.2
pig$valueUP2 = pmax(pig$mirrorUP,0.4)-0.4
pig$levelDOWN = pig$mirrorDOWN %/% 0.2
pig$valueDOWN0 = pmin(pig$mirrorDOWN,0.2)
pig$valueDOWN1 = pmin(pmax(pig$mirrorDOWN,0.2),0.4)-0.2
pig$valueDOWN2 = pmax(pig$mirrorDOWN,0.4)-0.4
idx2 = setdiff(which(abs(diff(pig$levelUP))==1),which(pig$TIME %in% (1:4*48.00)))
for (i in 1:length(idx2)){
  if (any(pig$levelUP[c(idx2[i],idx2[i]+1)]==2)){
    wt = (0.4-pig$mirrorUP[idx2[i]])/diff(pig$mirrorUP[0:1+idx2[i]])
    pig=rbind(pig,data.frame(TIME=pig$TIME[idx2[i]]*(1-wt)+pig$TIME[idx2[i]+1]*wt,variable=pig$variable[idx2[i]],value=0.9,sign=pig$sign[idx2[i]],mirrorUP=0.4,mirrorDOWN=0,levelUP=2,valueUP0=0.2,valueUP1=0.2,valueUP2=0,levelDOWN=0,valueDOWN0=0,valueDOWN1=0,valueDOWN2=0))
  } else {
    wt = (0.2-pig$mirrorUP[idx2[i]])/diff(pig$mirrorUP[0:1+idx2[i]])
    pig=rbind(pig,data.frame(TIME=pig$TIME[idx2[i]]*(1-wt)+pig$TIME[idx2[i]+1]*wt,variable=pig$variable[idx2[i]],value=0.7,sign=pig$sign[idx2[i]],mirrorUP=0.2,mirrorDOWN=0,levelUP=1,valueUP0=0.2,valueUP1=0,valueUP2=0,levelDOWN=0,valueDOWN0=0,valueDOWN1=0,valueDOWN2=0))
  }
}
idx3 = setdiff(which(abs(diff(pig$levelDOWN))==1),which(pig$TIME %in% (1:4*48.00)))
for (i in 1:length(idx3)){
  if (any(pig$levelDOWN[c(idx3[i],idx3[i]+1)]==2)){
    wt = (0.4-pig$mirrorDOWN[idx3[i]])/diff(pig$mirrorDOWN[0:1+idx3[i]])
    pig=rbind(pig,data.frame(TIME=pig$TIME[idx3[i]]*(1-wt)+pig$TIME[idx3[i]+1]*wt,variable=pig$variable[idx3[i]],value=0.1,sign=pig$sign[idx3[i]],mirrorUP=0,mirrorDOWN=0.4,levelUP=0,valueUP0=0,valueUP1=0,valueUP2=0,levelDOWN=2,valueDOWN0=0.2,valueDOWN1=0.2,valueDOWN2=0))
  } else {
    wt = (0.2-pig$mirrorDOWN[idx3[i]])/diff(pig$mirrorDOWN[0:1+idx3[i]])
    pig=rbind(pig,data.frame(TIME=pig$TIME[idx3[i]]*(1-wt)+pig$TIME[idx3[i]+1]*wt,variable=pig$variable[idx3[i]],value=0.3,sign=pig$sign[idx3[i]],mirrorUP=0,mirrorDOWN=0.2,levelUP=0,valueUP0=0,valueUP1=0,valueUP2=0,levelDOWN=1,valueDOWN0=0.2,valueDOWN1=0,valueDOWN2=0))
  }
}
idx1 = setdiff(which(abs(diff(pig$levelUP))==2),which(pig$TIME %in% (1:4*48.00)))
idx1 = idx1[1:2]
for (i in 1:length(idx1)){
  wt1 = (0.2-pig$mirrorUP[idx1[i]])/diff(pig$mirrorUP[0:1+idx1[i]])
  wt2 = (0.4-pig$mirrorUP[idx1[i]])/diff(pig$mirrorUP[0:1+idx1[i]])
  wt = c(wt1,wt2)
  pig=rbind(pig,data.frame(TIME=pig$TIME[idx1[i]]+wt,variable=pig$variable[idx1[i]],value=c(0.7,0.9),sign=pig$sign[idx1[i]],mirrorUP=c(0.2,0.4),mirrorDOWN=0,levelUP=c(1,2),valueUP0=0.2,valueUP1=c(0,0.2),valueUP2=0,levelDOWN=0,valueDOWN0=0,valueDOWN1=0,valueDOWN2=0))
}
pig=pig[order(pig$variable,pig$TIME),]
pig2=reshape2::melt(pig[,-c(3:7,11)],1:2)
names(pig2)[2:3] = c('V','level')
pig2$line = paste(pig2$V,pig2$level,sep=":")
pig2$V = factor(as.character(pig2$V),levels=paste0('V',4:1))
pdf("pipeline-02-horizon.pdf",width=5,height=3)
qplot(TIME, value, data=pig2, geom='area', fill=level, group=level, facets=V~.,position='identity',alpha=I(0.3)) + scale_y_continuous(breaks = c(0,0.1,0.2)) + scale_fill_manual(values = c("red","red", "red","blue","blue","blue")) + theme(legend.position='none')
dev.off()

pig = pigs[,c(1:3,7:8,10:11,7:8,10:11)]
names(pig)[8:11]=paste0("V",1:4)
for (i in 4:7) pig[,i] = (pig[,i]-min(pig[,i]))/diff(range(pig[,i]))
for (i in 4:7) pig[,i+4] = rowSums(pig[,4:i,drop=FALSE])
pig$V0 = 0
pig2=data.frame(t(apply(pig[,8:12],1,function(x){x-mean(range(x))})))
pig2$Time = pig$TIME
pig2 = pig2[,c(6,5,1:4)]
pig = reshape2::melt(pig2,1)
pig$variable = factor(as.character(pig$variable),levels=paste0('V',4:0))
pig$value2 = pig$value[c(49:nrow(pig),1:48)]
pig$variable2 = pig$variable[c(49:nrow(pig),1:48)]
pig = pig[1:(nrow(pig)-48),]
pig$variable = pig$variable2
pdf("pipeline-02-themeriver.pdf",width=5,height=3)
ggplot(pig, aes(x=Time,group=variable,fill=variable))+geom_ribbon(aes(ymin=value, ymax=value2))+theme(legend.position='none')+ylab("Value")
dev.off()


########## Figure 3 ############################################################
pdf("pipeline-03-polarline.pdf",width=5,height=5)
qplot(TimeIndx, ts, data=nasa2221, geom=c('line','point'), ylim=c(-0.1,1), xlab='',ylab='') +
  coord_polar() +
  theme(axis.text=element_blank(), axis.ticks=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor = element_blank())
dev.off()

pdf("pipeline-03-polarperiod.pdf",width=5,height=5)
qplot(Month, ts, data=nasa2221, geom=c('line','point'),group=Year,ylim=c(-0.1,1), xlab='',ylab='') + coord_polar() + 
  theme(axis.text=element_blank(), axis.ticks=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor = element_blank())
dev.off()

pdf("pipeline-03-spiral.pdf",width=5,height=5)
qplot(Month, TimeIndx, data=nasa2221, geom='rect', fill=ts, group=Year,
      xmin=Month-0.5, xmax=Month+0.5, ymin=TimeIndx-5, ymax=TimeIndx+5,
      xlab='',ylab='',ylim=c(-96,80)) +
  coord_polar() + scale_fill_gradient(low='black',high='yellow') +
  theme(axis.text=element_blank(), axis.ticks=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),
        legend.position='none')
dev.off()


########## Figure 6, 12 ########################################################
qlynx <- qdata(data.frame(Time=1:114, lynx))
qtime(Time, lynx, qlynx, shift=1:13, series.stats=FALSE)


########## Figure 7 ############################################################
qpig <- qdata(pigs)
qtime(TIME, c("GILTS","PROFIT","PRODUCTION","HERDSZ"), qpig, shift=c(1,4))


########## Figure 8, 13, 14 ####################################################
nasa21 = subset(nasa, Gridx %in% 19:21 & Gridy == 21)
nasa21$Year = factor(nasa21$Year)
nasa21$Gridx = factor(as.integer(factor(nasa21$Gridx)))
colnames(nasa21)[c(1,6,9,14)]=c('Individual','Time','A','B')
qnasa = qdata(nasa21)
qtime(Time,c(A,B),qnasa,vdiv=Individual,shift=c(1,12))


########## Figure 9 ############################################################
nasa$Gridx = factor(nasa$Gridx)
nasa$Gridy = factor(25-nasa$Gridy)
nasa$Year = factor(nasa$Year)
qnasa = qdata(nasa)
qtime(TimeIndx,ts,qnasa,hdiv=Gridx,vdiv=Gridy,shift=c(1,12),asp=1)


########## Figure 10 ###########################################################
library(nlme)
Remi = Remifentanil[complete.cases(Remifentanil) & Remifentanil$Time<=41,]
Remi$age = cut(Remi$Age,c(18,40,65,90))
Remi$Height = cut(Remi$Ht,c(153,163,173,183,193))
Remi$Weight = cut(Remi$Wt,c(40,70,80,110))
Remi$ID = factor(Remi$ID)
names(Remi)[c(4,7,13)] = c('Concentration','age','Age')
qRemi = qdata(Remi)
qtime(Time, Concentration, qRemi, vdiv=c(Age,Weight,ID), hdiv=c(Sex,Height), infolab=c('Sex','age','Ht','Wt'),asp=1.8)


########## Figure 11 ###########################################################
nasa2221 = subset(nasa, Gridx==22 & Gridy == 21)
nasa2221$Year = factor(as.integer(factor(nasa2221$Year)))
colnames(nasa2221)[c(6,7,9)]=c('Time','Period','Value')
nasa2221$Value = (nasa2221$Value - min(nasa2221$Value))/(diff(range(nasa2221$Value)))
qnasa2221 = qdata(nasa2221)
qtime(Time,Value,qnasa2221,shift=c(1,12),series.stats=FALSE)
qtime(Time,Value,qnasa2221,shift=c(1,12),vdiv=Period)


########## Figure 16 ###########################################################
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
qtime(days, FluSearches, data=qflu, vdiv="State",shift=c(1,7,28,35,91),infolab='Date')

########## Figure 18 ###########################################################

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


########## Figure ? ############################################################
dat = data.frame(x=1:6+15, y=c(3,5,1,2.6,4.8,1.6))
plot(dat,type='b',xlab='time',ylab='value',xlim=c(min(dat$x),max(dat$x)))

for (cutx in max(dat$x)-1:3){
  Sys.sleep(1)
  abline(v=cutx+0.5,lty=2,col=2)
  Sys.sleep(1)
  tmpdat = dat
  tmpdat$x[dat$x>cutx] = dat$x[dat$x>cutx] - (ceiling((dat$x[dat$x>cutx]-min(dat$x)+1)/(cutx-min(dat$x)+1))-1)*(cutx-min(dat$x)+1)
  tmpdat=rbind(tmpdat[dat$x<=cutx,],c(NA,NA),tmpdat[dat$x>cutx,])
  plot(tmpdat,type='b',xlab='time',ylab='value',xlim=c(min(dat$x),max(dat$x)))
}
