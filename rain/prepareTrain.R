##############################
### Large script for moving through aggregations by applying a similar methodology to each
### Appears as a large script because that's how it really was worked on; each pattern was 
###  repeated on new features
###
### Makes a lot of assumptions about having data in place. Working to streamline this all
###  to look like a production process. But getting "something" out now.
##############################

source("rain_functions.R")

q<-lapply(train.RadarQualityIndex, function(x) as.numeric(removeInvalids(x)))

HybridScan<-lapply(train.HybridScan, function(x) as.numeric(removeInvalids(x)))
wtHybridScan<-mapply(function(x,y) getAggregates(x,"weightedMean",y),x=HybridScan,y=q)
wtHybridScan[is.na(wtHybridScan)]<-NA
meanHybridScan<-mapply(function(x,y) getAggregates(x,"mean",y),x=HybridScan,y=q)
meanHybridScan[is.na(meanHybridScan)]<-NA
wtSquaredHybridScan<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=HybridScan,y=q)
wtSquaredHybridScan[is.na(wtSquaredHybridScan)]<-NA
rm(HybridScan)


minDistanceToRadar<-unlist(lapply(train.DistanceToRadar,min))

Reflectivity<-lapply(train.Reflectivity, function(x) as.numeric(removeInvalids(x)))
wtReflectivity<-mapply(function(x,y) getAggregates(x,"weightedMean",y),x=Reflectivity,y=q)
wtReflectivity[is.na(wtReflectivity)]<-NA
meanReflectivity<-mapply(function(x,y) getAggregates(x,"mean",y),x=Reflectivity,y=q)
meanReflectivity[is.na(meanReflectivity)]<-NA
rm(Reflectivity); gc()

ReflectivityQC<-lapply(train.ReflectivityQC, function(x) as.numeric(removeInvalids(x)))
wtReflectivityQC<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=ReflectivityQC,y=q)
wtReflectivityQC[is.na(wtReflectivityQC)]<-NA
meanReflectivityQC<-mapply(function(x,y) getAggregates(x,"mean",y),x=ReflectivityQC,y=q)
meanReflectivityQC[is.na(meanReflectivityQC)]<-NA
rm(ReflectivityQC); gc()

Zdr<-lapply(train.Zdr, function(x) as.numeric(removeInvalids(x)))
wtZdr<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=Zdr,y=q)
wtZdr[is.na(wtZdr)]<-NA
meanZdr<-mapply(function(x,y) getAggregates(x,"mean",y),x=Zdr,y=q)
meanZdr[is.na(meanZdr)]<-NA
rm(Zdr); gc()

LogWaterVolume<-lapply(train.LogWaterVolume, function(x) as.numeric(removeInvalids(x)))
wtLogWaterVolume<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=LogWaterVolume,y=q)
wtLogWaterVolume[is.na(wtLogWaterVolume)]<-NA
meanLogWaterVolume<-mapply(function(x,y) getAggregates(x,"mean",y),x=LogWaterVolume,y=q)
meanLogWaterVolume[is.na(meanLogWaterVolume)]<-NA
rm(LogWaterVolume); gc()

Composite<-lapply(train.Composite, function(x) as.numeric(removeInvalids(x)))
wtComposite<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=Composite,y=q)
wtComposite[is.na(wtComposite)]<-NA
meanComposite<-mapply(function(x,y) getAggregates(x,"mean",y),x=Composite,y=q)
meanComposite[is.na(meanComposite)]<-NA
rm(Composite); gc()

RR1<-lapply(train.RR1, function(x) as.numeric(removeInvalids(x)))
wtRR1<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=RR1,y=q)
wtRR1[is.na(wtRR1)]<-NA
meanRR1<-mapply(function(x,y) getAggregates(x,"mean",y),x=RR1,y=q)
meanRR1[is.na(meanRR1)]<-NA
rm(RR1); gc()

RR2<-lapply(train.RR2, function(x) as.numeric(removeInvalids(x)))
wtRR2<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=RR2,y=q)
wtRR2[is.na(wtRR2)]<-NA
meanRR2<-mapply(function(x,y) getAggregates(x,"mean",y),x=RR2,y=q)
meanRR2[is.na(meanRR2)]<-NA
rm(RR2); gc()

RR3<-lapply(train.RR3, function(x) as.numeric(removeInvalids(x)))
wtRR3<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=RR3,y=q)
wtRR3[is.na(wtRR3)]<-NA
meanRR3<-mapply(function(x,y) getAggregates(x,"mean",y),x=RR3,y=q)
meanRR3[is.na(meanRR3)]<-NA
rm(RR3); gc()

RhoHV<-lapply(train.RhoHV, function(x) as.numeric(removeInvalids(x)))
wtRhoHV<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=RhoHV,y=q)
wtRhoHV[is.na(wtRhoHV)]<-NA
meanRhoHV<-mapply(function(x,y) getAggregates(x,"mean",y),x=RhoHV,y=q)
meanRhoHV[is.na(meanRhoHV)]<-NA
rm(RhoHV); gc()

Velocity<-lapply(train.Velocity, function(x) as.numeric(removeInvalids(x)))
wtVelocity<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=Velocity,y=q)
wtVelocity[is.na(wtVelocity)]<-NA
meanVelocity<-mapply(function(x,y) getAggregates(x,"mean",y),x=Velocity,y=q)
meanVelocity[is.na(meanVelocity)]<-NA
rm(Velocity); gc()

modRain<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x) %in% c(1,2),1,0))))
heavyRain<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x)==3,1,0))))
rainHail<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x)==4,1,0))))
bigDrops<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x)==5,1,0))))
snow<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x) %in% c(10,11),1,0))))
graupel<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x) %in% c(13,14),1,0))))
validReadings<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x) >0,1,0))))
allPrecipitation<-unlist(lapply(train.HydrometeorType,function(x) sum(ifelse(as.numeric(x) %in% c(1,2,3,4,5,10,11,12,13,14),1,0))))

#train.MassWeightedMean<-lapply(as.character(train[,MassWeightedMean]),function(x) as.numeric(strsplit(x," ")[[1]])); train[,MassWeightedMean:=NULL]; gc()
#train.MassWeightedSD<-lapply(as.character(train[,MassWeightedSD]),function(x) as.numeric(strsplit(x," ")[[1]])); train[,MassWeightedSD:=NULL]; gc()
#train.HydrometeorType<-lapply(as.character(train[,HydrometeorType]),function(x) as.numeric(strsplit(x," ")[[1]])); train[,HydrometeorType:=NULL]; gc()

maxRadarQualityIndex<-mapply(function(x,y) getAggregates(x,"max",y),x=q,y=q)
maxRadarQualityIndex[is.na(maxRadarQualityIndex)]<-NA
avgRadarQualityIndex<-mapply(function(x,y) getAggregates(x,"mean",y),x=q,y=q)
avgRadarQualityIndex[is.na(avgRadarQualityIndex)]<-NA
maxTimeToEnd<-mapply(function(x,y) getAggregates(x,"max",y),x=train.TimeToEnd,y=q)
maxTimeToEnd[is.na(maxTimeToEnd)]<-NA
minTimeToEnd<-mapply(function(x,y) getAggregates(x,"min",y),x=train.TimeToEnd,y=q)
minTimeToEnd[is.na(minTimeToEnd)]<-NA
diffTimeToEnd<-maxTimeToEnd-minTimeToEnd
diffTimeToEnd[is.na(diffTimeToEnd)]<-NA

t5<-unlist(lapply(train.TimeToEnd,function(x) max(ifelse(x>49 & x<61,1,0))))
t4<-unlist(lapply(train.TimeToEnd,function(x) max(ifelse(x>39 & x<51,1,0))))
t3<-unlist(lapply(train.TimeToEnd,function(x) max(ifelse(x>29 & x<41,1,0))))
t2<-unlist(lapply(train.TimeToEnd,function(x) max(ifelse(x>19 & x<31,1,0))))
t1<-unlist(lapply(train.TimeToEnd,function(x) max(ifelse(x>9 & x<21,1,0))))
t0<-unlist(lapply(train.TimeToEnd,function(x) max(ifelse(x<11,1,0))))
time10s<-t5+t4+t3+t2+t1+t0

load("RdataFiles/train.Reflectivity.Rdata")
load("RdataFiles/train.TimeToEnd.Rdata")
Ref<-lapply(train.Reflectivity, function(x) as.numeric(removeInvalids(x)))
r5<-unlist(mapply(function(x,y) max(ifelse(x>49 & x<61,y,0)),x=train.TimeToEnd,y=Ref))
r4<-unlist(mapply(function(x,y) max(ifelse(x>39 & x<50,y,0)),x=train.TimeToEnd,y=Ref))
r3<-unlist(mapply(function(x,y) max(ifelse(x>29 & x<40,y,0)),x=train.TimeToEnd,y=Ref))
r2<-unlist(mapply(function(x,y) max(ifelse(x>19 & x<30,y,0)),x=train.TimeToEnd,y=Ref))
r1<-unlist(mapply(function(x,y) max(ifelse(x>9 & x<20,y,0)),x=train.TimeToEnd,y=Ref))
r0<-unlist(mapply(function(x,y) max(ifelse(x<10,y,0)),x=train.TimeToEnd,y=Ref))
refMax10s<-r5+r4+r3+r2+r1+r0
refMaxSql10s<-r5^2+r4^2+r3^2+r2^2+r1^2+r0^2
save(refMax10s,file="RdataFiles/refMax10s.Rdata")
save(refMaxSql10s,file="RdataFiles/refMaxSql10s.Rdata")
rm(r5,r4,r3,r2,r1,r0,Ref,train.Reflectivity)


load("RdataFiles/train.RhoHV.Rdata")
Rho<-train.RhoHV
Rho5<-unlist(mapply(function(x,y) max(ifelse(x>49 & x<61,y,0)),x=train.TimeToEnd,y=Rho))
Rho4<-unlist(mapply(function(x,y) max(ifelse(x>39 & x<50,y,0)),x=train.TimeToEnd,y=Rho))
Rho3<-unlist(mapply(function(x,y) max(ifelse(x>29 & x<40,y,0)),x=train.TimeToEnd,y=Rho))
Rho2<-unlist(mapply(function(x,y) max(ifelse(x>19 & x<30,y,0)),x=train.TimeToEnd,y=Rho))
Rho1<-unlist(mapply(function(x,y) max(ifelse(x>9 & x<20,y,0)),x=train.TimeToEnd,y=Rho))
Rho0<-unlist(mapply(function(x,y) max(ifelse(x<10,y,0)),x=train.TimeToEnd,y=Rho))
RhoMax10s<-Rho5+Rho4+Rho3+Rho2+Rho1+Rho0
RhoMaxSq10s<-Rho5^2+Rho4^2+Rho3^2+Rho2^2+Rho1^2+Rho0^2
save(RhoMax10s,file="RdataFiles/RhoMax10s.Rdata")
save(RhoMaxSq10s,file="RdataFiles/RhoMaxSq10s.Rdata")
rm(Rho5,Rho4,Rho3,Rho2,Rho1,Rho0,Rho,train.RhoHV)


load("RdataFiles/train.Reflectivity.Rdata")
load("RdataFiles/train.DistanceToRadar.Rdata")
Sys.time(); vec<-mapply(function(x,y) getBestRadarScalar(cbind(x,y)),x=train.DistanceToRadar,y=train.Reflectivity); Sys.time()	## 14 minutes
Sys.time(); siteRef<-mapply(function(x,y) getAggregates(x,"weightedMean",y,2),x=train.Reflectivity,y=vec); Sys.time()	## 3 minutes

yBinary<-ifelse(Expected<=0,1,0)
colnames(yBinary)<-"yBinary"
##for H2O; for R, remove first two columns
x<-as.data.frame(cbind(Expected,yBinary,wtHybridScan,meanHybridScan,wtSquaredHybridScan,minDistanceToRadar,wtReflectivity,meanReflectivity,
	wtReflectivityQC,meanReflectivityQC,meanLogWaterVolume,wtLogWaterVolume,meanZdr,wtZdr,
	wtComposite,meanComposite,wtRR1,meanRR1,wtRR2,meanRR2,wtRR3,meanRR3,wtRhoHV,meanRhoHV,wtVelocity,meanVelocity,
	modRain,heavyRain,rainHail,bigDrops,snow,graupel,validReadings,allPrecipitation,
	avgRadarQualityIndex,maxRadarQualityIndex,maxTimeToEnd,minTimeToEnd,diffTimeToEnd,time10s
	,refMax10s,refMaxSql10s,RhoMax10s,RhoMaxSq10s,siteRef
	))
x1<-x[1:(nrow(x)*0.9),]
x2<-x[(1+nrow(x1)):nrow(x),]

#if testing, this makes things easier:
 x0<-x2
