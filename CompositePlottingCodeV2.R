#----------------------------------------------#
#-- code for plots in dynamic modeling paper --#
#----------------------------------------------#
require(ggplot2)
require(reshape2)
require(lattice)
require(latticeExtra)


#--------------------------#
#-- 3D pop trajectories ---#
#--------------------------#
compd.data<-read.csv("work/StatProjects/Raina/sheep/datasets/compiled_data_summary_24June2012_krmmod2.csv",header=T,sep="")
pops.in<-c("Wenaha","BlackButte","Redbird","MuirCreek","BigCanyon","SheepMountain")
dat<-subset(compd.data,Pop %in% pops.in & is.na(YearsSinceInvasion2)==F)
tab1<-table(factor(dat$Pop),dat$YearsSinceInvasion2)
newPop<-as.numeric(factor(dat$Pop))
YearsSinceInvasion3<-as.numeric(dat$YearsSinceInvasion2)
PopEst<-dat$PopESt

pl<-cloud(x=as.numeric(newPop),y=YearsSinceInvasion3,z=PopEst,panel.3d.cloud=panel.3d.bars)

?panel.3dbars

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

CRratio<-ParamMat$Gamma/(ParamMat$Nu/10)
CRratio<-rep(CRratio,each=10)
pers.times<-ext.popsize<-rep(NA,length(rho.1.min.omeg))
 for(i in 1:dim(ParamMat)[1]){
   for(j in 1:10){
     load(paste("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/02August2012/PassBack/Simout_02August2012__omega_",i,"_",j,sep=""))
     pers.times[(i-1)*10+j]<-max(na.omit(SimTest$persistence))
     ext.popsize[(i-1)*10+j]<-ifelse(max(na.omit(SimTest$persistence))==0,SimTest$N[729],SimTest$N[max(na.omit(SimTest$persistence))] )
   }
 }

 dat<-data.frame(cbind(CRratio,pers.times,ext.popsize))
 names(dat)<-c("CRratio","pers.times","ext.popsize")
 
 propacute1<-ggplot(dat,aes(x=CRratio,y=pers.times))
 (propacute2<-propacute1+geom_point(aes(size=ext.popsize,colour=ext.popsize))+theme_bw()+xlab("proportion acute")+ylab("persistence time")+scale_colour_gradient2(low="blue",mid="red",high="yellow",midpoint=100)+coord_trans(xtrans="log")+scale_size(range=c(1,20)))
 
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
 
 
# #-- now make polygon data --#
# xs<-c(c(2:length(na.omit(q.025[1,])),rev(2:length(na.omit(q.025[1,])))),c(2:length(na.omit(q.025[2,])),rev(2:length(na.omit(q.025[2,])))),c(2:length(na.omit(q.025[3,])),rev(2:length(na.omit(q.025[3,])))),c(2:length(na.omit(q.025[4,])),rev(2:length(na.omit(q.025[4,])))))
# ys<-c(c(q.025[1,2:length(na.omit(q.025[1,]))],rev(q.975[1,2:length(na.omit(q.975[1,]))])),c(q.025[2,2:length(na.omit(q.025[2,]))],rev(q.975[2,2:length(na.omit(q.975[2,]))])),c(q.025[3,2:length(na.omit(q.025[3,]))],rev(q.975[3,2:length(na.omit(q.975[3,]))])),c(q.025[4,2:length(na.omit(q.025[4,]))],rev(q.975[4,2:length(na.omit(q.975[4,]))])))
# # value<-data.frame(value = rep(ParamMat$tau[paramsets[1]], 4))
# positions <- data.frame(id =c(rep(1, length(na.omit(q.025[1,]))*2-2),rep(2, length(na.omit(q.025[2,]))*2-2),rep(3, length(na.omit(q.025[3,]))*2-2),rep(4, length(na.omit(q.025[4,]))*2-2)),  x = xs, y=ys) 
# taus<-ParamMat$tau[paramsets[positions$id]]
# datapolytau1<-data.frame(positions,taus)
# names(datapolytau1)<-c("id","x","y","tau")
# datapolytau1$tau2<-factor(datapolytau1$tau,levels=c("0.01","0.005","0.001","5e-04"))
# 
## (p <- ggplot(datapolytau, aes(x=x, y=y)) + geom_polygon(aes(fill=as.factor(tau), group=id,alpha=.2))+scale_fill_brewer(type="qual",palette="Set1")+theme_bw()) 
# 
# (p <- ggplot(datapolytau1, aes(x=x, y=y)) + geom_polygon(aes(fill=factor(tau), group=id,alpha=.2))+scale_fill_brewer(type="qual",palette="Set1")+theme_bw()) 
# 
# 
 #-- now make polygon data --#
 xs<-c(c(2:length(na.omit(q.025[2,])),rev(2:length(na.omit(q.025[2,])))),c(2:length(na.omit(q.025[3,])),rev(2:length(na.omit(q.025[3,])))),c(2:length(na.omit(q.025[4,])),rev(2:length(na.omit(q.025[4,])))),c(2:length(na.omit(q.025[1,])),rev(2:length(na.omit(q.025[1,])))))
 ys<-c(c(q.025[2,2:length(na.omit(q.025[2,]))],rev(q.975[2,2:length(na.omit(q.975[2,]))])),c(q.025[3,2:length(na.omit(q.025[3,]))],rev(q.975[3,2:length(na.omit(q.975[3,]))])),c(q.025[4,2:length(na.omit(q.025[4,]))],rev(q.975[4,2:length(na.omit(q.975[4,]))])),c(q.025[1,2:length(na.omit(q.025[1,]))],rev(q.975[1,2:length(na.omit(q.975[1,]))])))
 # value<-data.frame(value = rep(ParamMat$tau[paramsets[1]], 4))
 positions <- data.frame(id =c(rep(2, length(na.omit(q.025[2,]))*2-2),rep(3, length(na.omit(q.025[3,]))*2-2),rep(4, length(na.omit(q.025[4,]))*2-2),rep(1, length(na.omit(q.025[1,]))*2-2)),  x = xs, y=ys) 
 taus<-ParamMat$tau[paramsets[positions$id]]
 datapolytau1<-data.frame(positions,taus)
 names(datapolytau1)<-c("id","x","y","tau")
 datapolytau1$tau2<-factor(datapolytau1$tau,levels=c("0.01","0.005","0.001","5e-04"))
 
 # (p <- ggplot(datapolytau, aes(x=x, y=y)) + geom_polygon(aes(fill=as.factor(tau), group=id,alpha=.2))+scale_fill_brewer(type="qual",palette="Set1")+theme_bw()) 
 
 (p <- ggplot(datapolytau1, aes(x=x, y=y)) + geom_polygon(aes(fill=factor(tau), group=id,alpha=.2))+scale_fill_brewer(type="qual",palette="Set1",name="tau")+theme_bw()+guides(alpha=F)+xlab("days")+ylab("proportion chronic")) 
 
 
 
#---------------------------------------#
#-- adult mortality by tau -------------#
#---------------------------------------#
 
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
 
 #-- need annual adult mortality rates --#
# paramsets<-c(1,3,5,7)
# ad.morts<-Ns<-array(NA,dim=c(4,70,floor(10000/365)))
# years<-
# tau<-
#
# for(i in 1:4){
#   for(j in 1:70){
#   load(paste("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/July30_2012/PassBack/Simout_30July2012__omega_",paramsets[i],"_",j,sep=""))
#    for(k in 1:floor(10000/365)){
#      ad.morts[i,j,k]<-sum(SimTest$AdultMort[((k-1)*365+210):(k*365+210)])
#      Ns[i,j,k]<-SimTest$N[((k-1)*365+210)]
#    }
#   }
# }
 
 paramsets<-c(1,3,5,7)
 ratio<-ad.morts<-years<-tau<-Ns<-matrix(NA,4*70,floor(10000/365))
 pers.times<-rep(NA,4*70)
  
   for(i in 1:4){
     for(j in 1:70){
       load(paste("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/July30_2012/PassBack/Simout_30July2012__omega_",paramsets[i],"_",j,sep=""))
       pers.times[(i-1)*70+j]<-max(na.omit(SimTest$persistence))
         for(k in 1:floor(10000/365)){
          ad.morts[(i-1)*70+j,k]<-sum(na.omit(SimTest$AdultMort[((k-1)*365+210):(k*365+210)]))
          Ns[(i-1)*70+j,k]<-SimTest$N[((k-1)*365+210)]
          tau[(i-1)*70+j,k]<-ParamMat$tau[paramsets[i]]
          years[(i-1)*70+j,k]<-k
          ratio[(i-1)*70+j,k]<-ifelse(Ns[(i-1)*70+j,k]<=5,NA,ad.morts[(i-1)*70+j,k]/Ns[(i-1)*70+j,k])
       }
     }
   }
 
 taus<-as.vector(tau)
 ratios<-as.vector(ratio)
 year<-as.vector(years)
 N<-as.vector(Ns)
 ad.mort<-as.vector(ad.morts)
 
 dat<-data.frame(cbind(taus,ratios,year,N,ad.mort))
# dat<-dat[complete.cases(dat),]
 admort1<-ggplot(dat,aes(x=year,y=ratios,colour=factor(taus)))
 (admort2<-admort1+geom_point(aes(colour=factor(tau),alpha=.1))+guides(alpha=F)+theme_bw()+scale_colour_brewer(type="qual",palette="Set1"))
 (admort2<-admort1+geom_point(aes(alpha=.1))+stat_smooth(aes(fill=factor(taus)),alpha=.5,method="loess",n=20)+guides(alpha=F)+theme_bw()+scale_colour_brewer(type="qual",palette="Set1")+scale_fill_brewer(type="qual",palette="Set1"))
 
#---------------------------------------#
#-- tau by persistence time ------------#
#---------------------------------------#
density()
 
#-----------------------------------------#
#-- first year adult mort by prop acute --#
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
 
 prop.acute<-ParamMat$rho*(1-ParamMat$omega)
 
 #ParamMat.small<-subset(ParamMat,tau==.001)
 paramsets<-c(5,6,13,14)
 ratio<-ad.morts<-years<-prop.acute<-Ns<-rep(NA,4*70)
 pers.times<-rep(NA,4*70)
 
 for(i in 1:4){
   for(j in 1:70){
     load(paste("work/StatProjects/Raina/sheep/Papers/DynamicModel/Code/IndividualTrackingModel/TwoSeasonModels_11May2012/TwoSeasonsGitRepo/July30_2012/PassBack/Simout_30July2012__omega_",paramsets[i],"_",j,sep=""))
     pers.times[(i-1)*70+j]<-max(na.omit(SimTest$persistence))
#     for(k in 1:floor(10000/365)){
       ad.morts[(i-1)*70+j]<-sum(na.omit(SimTest$AdultMort[730:(730+209)]))
       Ns[(i-1)*70+j]<-SimTest$N[730]
       prop.acute[(i-1)*70+j]<-ParamMat$rho[paramsets[i]]*(1-ParamMat$omega[paramsets[i]])
       ratio[(i-1)*70+j]<-ifelse(Ns[(i-1)*70+j]<=5,NA,ad.morts[(i-1)*70+j]/Ns[(i-1)*70+j])
#     }
   }
 }
 
 
 dat<-data.frame(cbind(prop.acute,ratio,Ns,ad.morts))
 # dat<-dat[complete.cases(dat),]
 admort1<-ggplot(dat,aes(x=factor(prop.acute),y=ratio))
# (admort2<-admort1+geom_point(aes(colour=factor(prop.acute),alpha=.1))+guides(alpha=F)+theme_bw()+scale_colour_brewer(type="qual",palette="Set1"))
# (admort2<-admort1+geom_point(aes(alpha=.1))+stat_smooth(aes(fill=factor(taus)),alpha=.5,method="loess",n=20)+guides(alpha=F)+theme_bw()+scale_colour_brewer(type="qual",palette="Set1")+scale_fill_brewer(type="qual",palette="Set1"))
 (admort2<-admort1+geom_point(aes(alpha=.1))+geom_boxplot()+guides(alpha=F)+theme_bw()+scale_colour_brewer(type="qual",palette="Set1")+xlab("proportion acute")+ylab("first year adult mortality"))
# (admort2<-admort1+geom_point(aes(colour=factor(prop.acute),alpha=.1))+stat_boxplot(aes(y=ratio,x=factor(prop.acute)))+guides(alpha=F)+theme_bw()+scale_colour_brewer(type="qual",palette="Set1"))
 
 
#-----------------------------------------#
#-- first year r est by prop acute -------#
#-----------------------------------------#