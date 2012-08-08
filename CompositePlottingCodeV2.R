#----------------------------------------------#
#-- code for plots in dynamic modeling paper --#
#----------------------------------------------#
require(ggplot2)
require(reshape2)
require(lattice)


#--------------------------#
#-- 3D pop trajectories ---#
#--------------------------#

#-----------------------------------------#
#-- conceptual figure (cumulative dose) --#
#-----------------------------------------#

load("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/July30_2012/PassBack/Simout_30July2012__omega_5_5")
dat<-data.frame(cbind(SimTest$SCount,SimTest$ECount,SimTest$AcutetotCount,SimTest$ChronicCount,SimTest$RtotCount))
names(dat)<-c("S","E","A","C","R")
dat2<-dat[c(2:max(na.omit(SimTest$persistence))),]
dim(dat2)
dat3<-data.frame(cbind(dat2,seq(2:max(na.omit(SimTest$persistence)))))
names(dat3)<-c(names(dat),"time")

dat4<-melt(dat3,id="time")
(con1<-ggplot(dat4, aes(time, value, fill=variable)) + geom_area()+scale_colour_brewer(type="qual",palette="Set1")+theme_bw())

  #---------------------------------------#
  #-- Annual averages within each group --#
  #---------------------------------------#
  Ss<-Es<-As<-Cs<-Rs<-rep(NA,floor(max(na.omit(SimTest$persistence))/365))
  for(i in 1:length(Ss)){
    Ss[i]<-mean(SimTest$SCount[((i-1)*365):(((i)*365)-1)])
    Es[i]<-mean(SimTest$ECount[((i-1)*365):(((i)*365)-1)])
    As[i]<-mean(SimTest$AcutetotCount[((i-1)*365):(((i)*365)-1)])
    Cs[i]<-mean(SimTest$ChronicCount[((i-1)*365):(((i)*365)-1)])
    Rs[i]<-mean(SimTest$RtotCount[((i-1)*365):(((i)*365)-1)])
  }

dat5<-data.frame(cbind(As,Cs,Es,Rs,Ss,c(1:(floor(max(na.omit(SimTest$persistence))/365)))))
names(dat5)<-c("A","C","E","R","S","year")
dat6<-melt(dat5,id="year")
conc2<-ggplot(dat6,aes(year,value,fill=variable))
(conc2a<-conc2+geom_area()+scale_fill_brewer(type="qual",palette="Set1")+theme_bw()+xlim(c(0,27))

#-----------------------------------------#
#--- conceptual figure (random carrier) --#
#-----------------------------------------#

load("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/02August2012/PassBack/Simout_02August2012__omega_5_5")

  #---------------------------------------#
  #-- Annual averages within each group --#
  #---------------------------------------#
  Ss<-Es<-As<-Cs<-Rs<-rep(NA,floor(max(na.omit(SimTest$persistence))/365))
  for(i in 1:length(Ss)){
    Ss[i]<-mean(SimTest$SCount[((i-1)*365):(((i)*365)-1)])
    Es[i]<-mean(SimTest$ECount[((i-1)*365):(((i)*365)-1)])
    As[i]<-mean(SimTest$AcutetotCount[((i-1)*365):(((i)*365)-1)])
    Cs[i]<-mean(SimTest$ChronicCount[((i-1)*365):(((i)*365)-1)])
    Rs[i]<-mean(SimTest$RtotCount[((i-1)*365):(((i)*365)-1)])
  }

  dat5<-data.frame(cbind(As,Cs,Es,Rs,Ss,c(1:(floor(max(na.omit(SimTest$persistence))/365)))))
  names(dat5)<-c("A","C","E","R","S","year")
  dat6<-melt(dat5,id="year")
  conc2<-ggplot(dat6,aes(year,value,fill=variable))
  (conc2a<-conc2+geom_area()+scale_fill_brewer(type="qual",palette="Set1")+theme_bw()+xlim(c(0,27)))


#-----------------------------------------#
#-- persitence time/pop size/prop acute --#
#-----------------------------------------#
  
model<-c("CD")
timesteps<-10000
BirthRate<-.85
SexRatio<-.5
n<-c(100)
Recr<-.4
LambTransmissionProb<-c(.9)
SLSdecrease<-c(.8)
chronicdose<-.1
xi<-1/30
eta<-1/6
Nu<-exp(seq(log(1/2),log(1/365),length.out=5))
Gamma<-exp(seq(log(1/90),log(1/1000),length.out=10))
Alpha<-c(.9999)
rho<-c(.9999)
tau<-c(.0005)
AlphaChronic<-0
chronicdecrease<-0
PropRecovered<-0
ngroups<-2
GammaLamb<-.02
contactnumber<-1
LambcontactnumberIn<-.5
omega<-c(.5)

ParamMat<-expand.grid(list(model=model,
                           timesteps=timesteps,
                           BirthRate=BirthRate,
                           SexRatio=SexRatio,
                           n=n,
                           Recr=Recr,
                           LambTransmissionProb=LambTransmissionProb,
                           SLSdecrease=SLSdecrease,
                           chronicdose=chronicdose,
                           xi=xi,
                           eta=eta,
                           Gamma=Gamma,
                           Nu=Nu,
                           Alpha=Alpha,
                           rho=rho,
                           tau=tau,
                           AlphaChronic=AlphaChronic,
                           chronicdecrease=chronicdecrease,
                           PropRecovered=PropRecovered,
                           ngroups=ngroups,
                           GammaLamb=GammaLamb,
                           contactnumber=contactnumber,
                           LambcontactnumberIn=LambcontactnumberIn,
                           omega=omega))

  
#-----------------------------------------#
#-- prop chronic figure ------------------#
#-----------------------------------------#


#---------------------------------------#
#-- tau by persistence time ------------#
#---------------------------------------#

#-----------------------------------------#
#-- first year adult mort by prop acute --#
#-----------------------------------------#

#-----------------------------------------#
#-- first year r est by prop acute -------#
#-----------------------------------------#