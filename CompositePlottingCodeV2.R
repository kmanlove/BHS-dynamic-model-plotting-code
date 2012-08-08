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
 Gamma<-c(1/365)
 Nu<-1/150
 Alpha<-c(.9999)
 rho<-c(.9999,.5)
 tau<-c(.01,.005,.001,.0005)
 AlphaChronic<-0
 chronicdecrease<-0
 PropRecovered<-0
 ngroups<-2
 GammaLamb<-.02
 contactnumber<-1
 LambcontactnumberIn<-.5
 omega<-c(.5,.25)
 
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
 
Ns<-array(NA,dim=c(4,70,10000))
Cs<-array(NA,dim=c(4,70,10000))
ratios<-array(NA,dim=c(4,70,10000))
paramsets<-c(1,3,5,7)
 for(i in 1:4){
   for(j in 1:70){
     load(paste("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/July30_2012/PassBack/Simout_30July2012__omega_",paramsets[i],"_",j,sep=""))
      for(k in 2:max(na.omit(SimTest$persistence))){
         Ns[i,j,k]<-SimTest$N[k]
         Cs[i,j,k]<-SimTest$ChronicCount[k]
         ratios[i,j,k]<-Cs[i,j,k]/Ns[i,j,k]
     }
   }
 }
 
q.025<-q.975<-matrix(NA,4,10000)
 for(i in 1:4){
   for(k in 1:10000){
     q.025[i,k]<-quantile(na.omit(ratios[i,,k]),.025)
     q.975[i,k]<-quantile(na.omit(ratios[i,,k]),.975)
   }
 }
 
taus<-rbind(rep(ParamMat$tau[paramsets[1]],10000),rep(ParamMat$tau[paramsets[2]],10000),rep(ParamMat$tau[paramsets[3]],10000),rep(ParamMat$tau[paramsets[4]],10000))
 
propdat<-data.frame(cbind(as.vector(taus),as.vector(q.025),as.vector(q.975)))
names(propdat)<-c("tau","q.025","q.975") 
 #-- now make polygon data --#
 xs<-c(1:length(na.omit(q.025[1,])),rev(1:length(na.omit(q.025[1,]))))
 ys<-c(q.025[1,1:length(na.omit(q.025[1,]))],rev(q.975[1,1:length(na.omit(q.975[1,]))]))
# value<-data.frame(value = rep(ParamMat$tau[paramsets[1]], 4))
 positions <- data.frame(id = rep(1, 4),  x = xs, y=ys) 
 datapolytau1<-data.frame(positions,rep(ParamMat$tau[paramsets[1]], dim(positions)[1]))
 names(datapolytau1)<-c("id","x","y","tau")
 
 (p <- ggplot(datapolytau1, aes(x=x, y=y)) + geom_polygon(aes(fill=tau, group=id))) 
 
 
 #-- now make polygon data --#
 xs<-c(1:length(na.omit(q.025[1,])),rev(1:length(na.omit(q.025[1,]))))
 ys<-c(q.025[1,1:length(na.omit(q.025[1,]))],rev(q.975[1,1:length(na.omit(q.975[1,]))]))
 # value<-data.frame(value = rep(ParamMat$tau[paramsets[1]], 4))
 positions <- data.frame(id = rep(1, 4),  x = xs, y=ys) 
 datapolytau1<-data.frame(positions,rep(ParamMat$tau[paramsets[1]], dim(positions)[1]))
 names(datapolytau1)<-c("id","x","y","tau")
 
 (p <- ggplot(datapolytau1, aes(x=x, y=y)) + geom_polygon(aes(fill=tau, group=id))) 
 
#---------------------------------------#
#-- tau by persistence time ------------#
#---------------------------------------#
density()
 
#-----------------------------------------#
#-- first year adult mort by prop acute --#
#-----------------------------------------#

#-----------------------------------------#
#-- first year r est by prop acute -------#
#-----------------------------------------#