###################################################################
## Function Definitions
##  these are very hard-coded to work with this data set
##  simply here to save typing, not much for modularity/flexibility
###################################################################

getOneSec<-function(person,session){
    ##fread a given person ID and session ID (character IDs, not integers)
    ##find all feedback events, get 1.000 seconds that follow
    ##retain the Cz and EOG channels
    ##returns data.frame with session, person, feedback id, Cz and EOG channel data
    library(data.table)
    fileName<-paste0("train/Data_",person,"_Sess",session,".csv")
    a<-fread(fileName)
    a[,rowId:=seq(1:nrow(a))]
    feedBacks<-a[FeedBackEvent==1,rowId]
    feedBackNum<-1
    returnSet<-a[feedBacks[feedBackNum]:(feedBacks[feedBackNum]+200),list(Cz,EOG,feedBackNum)]
    for(feedBackNum in 2:length(feedBacks)){returnSet<-rbind(returnSet,a[feedBacks[feedBackNum]:(feedBacks[feedBackNum]+200),list(Cz,EOG,feedBackNum)])}
    returnSet[,Person:=person]
    returnSet[,Session:=session]
    returnSet[,IdFeedBack:=paste0(Person,"_Sess",Session,"_FB",ifelse(feedBackNum<10,"00",ifelse(feedBackNum<100,"0","")),feedBackNum)]
    return(as.data.frame(returnSet)) 
}

getMovingAvgs<-function(vec){
    ## originally picked specific vectors, but after comparing; always used p15
    globalMin<-min(vec)
    globalMax<-max(vec)
    scaled<-(globalMax-vec)/(globalMax-globalMin)
    p5<-rep(0,186)
    p15<-rep(0,186)
    for(i in 1:186){
        p5[i]<-mean(scaled[(i+10):(i+14)])
        p15[i]<-mean(scaled[i:(i+14)])
    }   
    return(as.data.frame(cbind(p5,p15)))
} 

fitGbm<-function(x,y,outType="auc",trees=100,shrink=0.05,no=10,depth=4,cvInt=1){
  ##runs a gbm and passes back either the score, the fit object, or a vector of predictions
  ## also performs very specifically leave-one-out CV
  library(gbm)
  library(Metrics)
  if(cvInt>=0){
    idx<-rep(FALSE,nrow(x))
    idx[(1+(cvInt-1)*340):(cvInt*340)]<-TRUE
    x1<-x[idx==FALSE,]
    y1<-y[idx==FALSE]
    x2<-x[idx,]
    y2<-y[idx]
  }
  if(cvInt<0){x1<-x; x2<-x; y1<-y; y2<-y}
  fit<-gbm.fit(x=x1,y=y1,distribution="adaboost",n.trees=trees,shrinkage=shrink,n.minobsinnode=no,interaction.depth=depth)
  p<-predict(fit,newdata=x2,n.trees=trees,type="response")
  if(outType=="auc"){print(summary(fit,plotit=FALSE)); return(auc(y2,p))}
  if(outType=="predictions"){return(p)}
  if(outType=="model"){return(fit)}
}

prepareFeatures<-function(personSession,depth=60,vec=1){
    ## given session data, calculate some hand-crafted features that seemed
    ##  interesting when looking at plots
    ## stated AUCs are against the training set, in its entirety, I believe; so probably higher than what would translate to new data
    for(i in 1:depth){
        b<-getMovingAvgs(personSession[(1+((i-1)*201)):(i*201),vec])
        tmp<-cbind(
        sum(b[1:92,2])-sum(b[93:184,2]) ##auc 0.625
        ,findMin(b[,2],20,70)   ##auc 0.604
        ,findDepthToMinDrop(b[,2],20,70)    ##auc 0.5809985
        ,findDepthAfterMinDrop(b[,2],20,70) ##auc 0.5875286
        ,findDepthAfterMinDrop(b[,2],90,170)    ##auc 0.5737571
        ,b[nrow(b),2]-b[1,2]    ##auc 0.5654925
        ,sd(b[,2])  ##auc 0.55485 (1-auc)
        ,getVolatility(b[,2],110,160)   ##auc 0.5299781
        ,sd(b[20:70,2])/getVolatility(b[,2],20,70)  ##auc 0.5608
        ,personSession[(1+((i-1)*201)),3]
        ,personSession[(1+((i-1)*201)),5]
        ,t(b[1:184,2])
        ,min(personSession[(1+((i-1)*201)),vec])
        ,max(personSession[(1+((i-1)*201)),vec])
        ,max(personSession[(1+((i-1)*201)),vec])-min(personSession[(1+((i-1)*201)),vec])
        )
        if(i==1){x<-tmp}
        if(i>1){x<-rbind(x,tmp)}
    }
    return(x)
}

findMin<-function(vec,cutMin=1,cutMax=10000){
 vec<-vec[cutMin:(pmin(length(vec),cutMax))]
 id<-seq(1:length(vec))
 return(id[vec==min(vec)])
}

findDepthToMinDrop<-function(vec,cutMin=1,cutMax=10000){
 vec<-vec[cutMin:(pmin(length(vec),cutMax))]
 id<-seq(1:length(vec))
 end<-id[vec==min(vec)]
 return(max(vec[pmax(1,end-30):end])-vec[end])
}

findDepthAfterMinDrop<-function(vec,cutMin=1,cutMax=10000){
 vec<-vec[cutMin:(pmin(length(vec),cutMax))]
 id<-seq(1:length(vec))
 end<-id[vec==min(vec)]
 return(max(vec[pmin(length(vec),end+30):end])-vec[end])
}

getVolatility<-function(vec,cutMin=1,cutMax=10000){
 vec<-vec[cutMin:(pmin(length(vec),cutMax))]
 return(sd(vec[2:length(vec)]-vec[1:(length(vec)-1)]))
}


getFeedBacks<-function(person,session,cap=100){
	##fread a given person ID and session ID (character IDs, not integers)
	##return offsets of the FeedBackEvents
	library(data.table)
	fileName<-paste0("train/Data_",person,"_Sess",session,".csv")
	a<-fread(fileName)
	a[,rowId:=seq(1:nrow(a))]
	feedBacks<-a[FeedBackEvent==1,rowId]
	return(feedBacks[1:pmin(cap,length(feedBacks))])
}

createMagic<-function(v,cutoff){
  ## take in a vector of feedback differences
  ## produce a vector where negative means likely wrong; positive is likely right
  ##   and the values are scaled by the implied accuracy rate from the feedback timings
  magic<-ifelse(v>=70,0,ifelse(v>cutoff,-1,1))
  net<-sum(magic)
  scalar<-ifelse(net>75,4,
          ifelse(net>56,3,
          ifelse(net>12,2,1)))
  magic<-magic*scalar
  return(magic)
}



###############################
##	Create Train Data Frame
###############################

##get main features: 11 hand-crafted and higher-level features (subject/session) plus ~180 15-period averaged min/max scaled Cz signals
##currently set to use vector 1 (Cz); later used 2 (EOG), but minimal improvement
personList<-c("S02","S06","S07","S11","S12","S13","S14","S16","S17","S18","S20","S21","S22","S23","S24","S26")
sessionList<-c("01","02","03","04","05")
depthList<-c(60,60,60,60,100)
for(i in 1:length(personList)){
  for(j in 1:length(sessionList)){
    if(i*j==1){xAll<-prepareFeatures(getOneSec(personList[i],sessionList[j]),depthList[j],1)}
    if(i*j>1){xAll<-rbind(xAll,prepareFeatures(getOneSec(personList[i],sessionList[j]),depthList[j],1))}
    print(dim(xAll))
  }
}


## use coefficients of PLS model associating EOG with features from Matlab time-frequency kernel 
for(i in 1:length(personList)){
  for(j in 1:length(sessionList)){
    fileBase<-paste0("pls/Data_",personList[i],"_Sess",sessionList[j],"_pls.csv")
    if(i*j==1){plsAll<-read.csv(fileBase,header=FALSE)}
    if(i*j>1){plsAll<-rbind(plsAll,read.csv(fileBase,header=FALSE))}
  }
}

## get timings between subjects
for(i in 1:length(personList)){
  for(j in 1:length(sessionList)){
	if(i*j==1){xFB<-getFeedBacks(personList[i],sessionList[j],depthList[j])}
	if(i*j>1){xFB<-c(xFB,getFeedBacks(personList[i],sessionList[j],depthList[j]))}
  }
  print(i)
}
xFBDiff<-pmax(0,c(0,xFB[2:length(xFB)]-xFB[1:(length(xFB)-1)]))
xFBDiff[xFBDiff==0]<-4500
xFBDiff<-round(xFBDiff/50,0)

trainAll<-cbind(xAll,plsAll)
trainAll$xFBDiff<-xFBDiff

##count up the zones for spelling
spellCheck<-c(rep(65,340),rep(93,340),rep(88,340),rep(62,340),rep(60,340),rep(50,340),rep(79,340),rep(63,340),rep(56,340),rep(70,340),
 rep(62,340),rep(88,340),rep(94,340),rep(73,340),rep(68,340),rep(73,340))
trainAll$spellCheck<-spellCheck
delay<-rep(0,nrow(trainAll))
## attempt to encode not the straight numbers from spellCheck, but a great/good/average/bad speller type encoding
##  the intent is to scale to unseen data (test has some really bad ones)
for(i in 1:15){delay[(241+(i-1)*340):(i*340)]<-trainAll$spellCheck[(241+(i-1)*340):(i*340)]*10000+round(delay[(242+(i-1)*340):(1+i*340)]/4,0)}
delay[5341:5439]<-trainAll$spellCheck[5341:5439]*10000+round(delay[5342:5440]/4,0)	##for i = 16
delay<-as.factor(as.character(delay))

x2<-as.data.frame(unlist(sapply(trainAll,function(x) as.numeric(as.character(x)))))
x2$delay<-delay

trainSess5<-c(0,1600)
for(i in 1:16){trainSess5[(1+(i-1)*100):(i*100)]<-xFBDiff[(241+(i-1)*340):(i*340)]}
for(i in 1:length(trainSess5)){if(i%%5==1) trainSess5[i]<-trainSess5[i]-20}

xMagic<-rep(0,nrow(x2))
xSingleMagic<-rep(0,nrow(x2))
adjFbDiff<-x2$xFBDiff

trainCuts<-c(   37,         40,40,         42,42,44,44,   45,46,47,  47,47,48,51,52,   52)
for(i in 1:16){
  m1<-createMagic(trainSess5[(1+(i-1)*100):(i*100)],trainCuts[i])
  singleMagic<-pmax(5,round(sum(pmax(0,m1)/max(m1))/10,0))
  xMagic[(241+(i-1)*340):(i*340)]<-m1
  xSingleMagic[(1+(i-1)*340):(i*340)]<-singleMagic
  adjFbDiff[(241+(i-1)*340):(i*340)]<-0
}

x2$xMagic<-as.factor(xMagic)
x2$xSingleMagic<-xSingleMagic
x2$xxAdjFBDiff<-adjFbDiff

## fix alignment problem
x2$xMagic[1:5439]<-x2$xMagic[2:5440]


## add EOG/Cz differences
maxLoop<-nrow(xAll)
for(i in 1:maxLoop){
	a<-as.numeric(xAll[i,12:190])	##eog
	b<-as.numeric(x2[i,12:190])	##cz
	a<-(a-min(a))/(max(a)-min(a))	##eog
	b<-(b-min(b))/(max(b)-min(b))	##cz
	diffs<-a-b	##eog-cz
	scalar<-1-(max(diffs)/100)
	if(i==1){diff2<-t(as.data.frame(b-(a*scalar)))}
	if(i>1){diff2<-rbind(diff2,t(as.data.frame(b-(a*scalar))))}	
}

maxLoop<-nrow(xAll)
for(i in 1:maxLoop){
	a<-as.numeric(xAll[i,12:190])	##eog
	b<-as.numeric(x2[i,12:190])	##cz
	a<-(a-min(a))/(max(a)-min(a))	##eog
	b<-(b-min(b))/(max(b)-min(b))	##cz
	straightDiff<-a-b
	if(i==1){straightDiffs<-t(as.data.frame(straightDiff))}
	if(i>1){straightDiffs<-rbind(straightDiffs,t(as.data.frame(straightDiff)))}	
}


## add new features
x4<-cbind(x2,diff2)

diffDf<-as.data.frame(straightDiffs); colnames(diffDf)<-paste0("diff",colnames(diffDf)); rownames(diffDf)<-NULL
x6<-cbind(x4,diffDf)


###############################
##	Create Test Data Frame
###############################

testPersonList<-c("S01","S03","S04","S05","S08","S09","S10","S15","S19","S25")
sessionList<-c("01","02","03","04","05")
depthList<-c(60,60,60,60,100)
for(i in 1:length(testPersonList)){
  for(j in 1:length(sessionList)){
    if(i*j==1){xAllTest<-prepareFeatures(getOneSec(testPersonList[i],sessionList[j]),depthList[j])}
    if(i*j>1){xAllTest<-rbind(xAllTest,prepareFeatures(getOneSec(testPersonList[i],sessionList[j]),depthList[j]))}
    print(dim(xAllTest))
  }
}


for(i in 1:length(testPersonList)){
  for(j in 1:length(sessionList)){
    fileBase<-paste0("pls/Data_",testPersonList[i],"_Sess",sessionList[j],"_pls.csv")
    if(i*j==1){plsAllTest<-read.csv(fileBase,header=FALSE)}
    if(i*j>1){plsAllTest<-rbind(plsAllTest,read.csv(fileBase,header=FALSE))}
  }
}


for(i in 1:length(testPersonList)){
  for(j in 1:length(sessionList)){
	if(i*j==1){xFBTest<-getFeedBacks(testPersonList[i],sessionList[j],depthList[j])}
	if(i*j>1){xFBTest<-c(xFBTest,getFeedBacks(testPersonList[i],sessionList[j],depthList[j]))}
  }
  print(i)
}
xTestFBDiff<-pmax(0,c(0,xFBTest[2:length(xFBTest)]-xFB[1:(length(xFBTest)-1)]))
xTestFBDiff[xFBDiff==0]<-4500
xTestFBDiff<-round(xTestFBDiff/50,0)

testAll<-cbind(xAllTest,plsAllTest)
testAll$xTestFBDiff<-xTestFBDiff

## isolate all session5's for analysis
testSess5<-c(0,1000)
for(i in 1:10){testSess5[(1+(i-1)*100):(i*100)]<-xTestFBDiff[(241+(i-1)*340):(i*340)]}

testAll$spellCheck<-trainAll$spellCheck[1:nrow(testAll)]
x3<-as.data.frame(unlist(sapply(testAll,function(x) as.numeric(as.character(x)))))
x3$delay<-x2$delay[1:nrow(x3)]
colnames(x3)<-colnames(x2)[1:ncol(x3)]

testCuts<-c( 36,   38,39,39,      40,41,42,            44,        47,               51   ) 
xMagicTest<-rep(0,nrow(x3))
xSingleMagicTest<-rep(0,nrow(x3))
adjFbDiffTest<-x3$xFBDiff

for(i in 1:10){
  m1<-createMagic(testSess5[(1+(i-1)*100):(i*100)],testCuts[i])
  singleMagic<-pmax(5,round(sum(pmax(0,m1)/max(m1))/10,0))
  xMagicTest[(241+(i-1)*340):(i*340)]<-m1
  xSingleMagicTest[(1+(i-1)*340):(i*340)]<-singleMagic
  adjFbDiffTest[(241+(i-1)*340):(i*340)]<-0
}

x3$xMagic<-as.factor(xMagicTest)
x3$xSingleMagic<-xSingleMagicTest
x3$xxAdjFBDiff<-adjFbDiffTest

## fix alignment problem
x3$xMagic[1:3399]<-x3$xMagic[2:3400]

  maxLoop<-nrow(x3)
  for(i in 1:maxLoop){
  	a<-as.numeric(x3[i,12:190])	##eog
  	b<-as.numeric(x3[i,12:190])	##cz
  	a<-(a-min(a))/(max(a)-min(a))	##eog
  	b<-(b-min(b))/(max(b)-min(b))	##cz
  	diffs<-a-b	##eog-cz
  	scalar<-1-(max(diffs)/100)
  	if(i==1){diff2Test<-t(as.data.frame(b-(a*scalar)))}
  	if(i>1){diff2Test<-rbind(diff2Test,t(as.data.frame(b-(a*scalar))))}	
  }

  maxLoop<-nrow(x3)
  for(i in 1:maxLoop){
  	a<-as.numeric(x3[i,12:190])	##eog
  	b<-as.numeric(x3[i,12:190])	##cz
  	a<-(a-min(a))/(max(a)-min(a))	##eog
  	b<-(b-min(b))/(max(b)-min(b))	##cz
  	straightDiff<-a-b
  	if(i==1){straightDiffsTest<-t(as.data.frame(straightDiff))}
  	if(i>1){straightDiffsTest<-rbind(straightDiffsTest,t(as.data.frame(straightDiff)))}	
  }

x5<-cbind(x3,diff2Test)
diffDfTest<-as.data.frame(straightDiffsTest); colnames(diffDfTest)<-paste0("diff",colnames(diffDfTest)); rownames(diffDfTest)<-NULL
x7<-cbind(x5,diffDfTest)


#############################################
##	Test Training by running
##	 leave-one-out (LOO) validation
##	 but waiting to check AUC against
##	 the overall set of points, as
##	 that will be different than averaging
##	 each subject's AUC
##	Most testing was done on smaller GBMs (100/0.1 vs 1000/0.01)
#############################################

l<-read.csv("TrainLabels.csv")
y<-l[,2]

cols<-c(1:11,77:88,132,125:128,64:66,120,165,199:205,402:404)

s1<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,1)
s2<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,2)
s3<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,3)
s4<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,4)
s5<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,5)
s6<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,6)
s7<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,7)
s8<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,8)
s9<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,9)
s10<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,10)
s11<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,11)
s12<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,12)
s13<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,13)
s14<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,14)
s15<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,15)
s16<-fitGbm(x6[,cols],y,"predictions",100,0.1,10,10,16)

sAll1k<-c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)
library(Metrics)
auc(y,sAll1k)


##################################
##	Generate submission
##################################
fullFit<-fitGbm(x6[,cols],y,"model",1000,0.01,10,10,-1)
summary(fullFit,plotit=FALSE)[1:30,]
pFinal<-predict(fullFit,newdata=x7[,cols],n.trees=1000,type="response")
plot(pFinal)
z<-read.csv("SampleSubmission.csv")
z[,2]<-pFinal

################################
## Post Processing
##
## The encoding for subject accuracy
##  is a conservative one, to ensure 
##  it would scale to new subjects.
##  This model alone was good for 3rd
##  place. But it jumped ~0.04 in AUC
##  to bump up and down all points for
##  a single subject, based on their 
##  rate of abnormal lenghts.

z[(1+0*340):(1*340),2]<-z[(1+0*340):(1*340),2]-0
z[(1+1*340):(2*340),2]<-z[(1+1*340):(2*340),2]-0.05
z[(1+2*340):(3*340),2]<-z[(1+2*340):(3*340),2]-0
z[(1+3*340):(4*340),2]<-z[(1+3*340):(4*340),2]-0.06
z[(1+4*340):(5*340),2]<-z[(1+4*340):(5*340),2]-0
z[(1+5*340):(6*340),2]<-z[(1+5*340):(6*340),2]+0.01
z[(1+6*340):(7*340),2]<-z[(1+6*340):(7*340),2]+0.02
z[(1+7*340):(8*340),2]<-z[(1+7*340):(8*340),2]+0.02
z[(1+8*340):(9*340),2]<-z[(1+8*340):(9*340),2]-0
z[(1+9*340):(10*340),2]<-z[(1+9*340):(10*340),2]-0.275
write.csv(z,"bci_submission.csv",row.names=FALSE,quote=FALSE)


## Table of rate of normal lengths vs GBM mean
## 1	0.67	0.64530
## 2	0.41	0.64200
## 3	0.83	0.69560
## 4	0.31	0.65300
## 5	0.68	0.71900
## 6	0.79	0.66480
## 7	0.92	0.93950
## 8	0.91	0.94390
## 9	0.64	0.69590
## 10	0.54	0.65420
