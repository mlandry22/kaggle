trimSpaces <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))

removeInvalids<-function(x,xNew="") return(gsub("999",xNew,
gsub("-99901",xNew,
gsub("-99902",xNew,
gsub("-99903",xNew,
gsub("-99900",xNew,
gsub("999.0",xNew,
gsub("-99901.0",xNew,
gsub("-99902.0",xNew,
gsub("-99903.0",xNew,
gsub("-99900.0",xNew,
gsub("nan",xNew,
 x))))))))))))

getAggregates<-function(x,type="weightedMean",wts=NA,exponent=1){
  ## pass in a vector of values and vector of weights
  ##   get back a single number, according to the specified type
  ##   type: mean, median, sd, max, min, meanSquared, sdSquared
  ## handles NAs, including fields with all NAs (returns NAs)

  if(type=="min") return(min(x^exponent,na.rm=TRUE))
  if(type=="max") return(max(x^exponent,na.rm=TRUE))
  if(type=="median") return(median(x^exponent,na.rm=TRUE))
  if(type=="sd") return(sd(x^exponent,na.rm=TRUE))
  if(type=="mean") return(mean(x^exponent,na.rm=TRUE))

  if(length(x)==sum(is.na(wts))) return(NA)
  wts[is.na(wts)]<-0
  x[is.na(x)]<-0
  if(type=="weightedMean") return(sum(x^exponent*wts)/sum(wts))

  return(NA)
}

getGlmByMM<-function(x1,x2,x3,raw1,raw2,threshold){
  library(glmnet)
  library(Metrics)
  library(Matrix)
  y1<-ifelse(raw1<=threshold,1,0)
  y2<-ifelse(raw2<=threshold,1,0)
  x1[is.na(x1)] <- 0
  x2[is.na(x2)] <- 0
  x3[is.na(x3)] <- 0
  g<-glmnet(as.matrix(x1),as.factor(y1),family=c("binomial"))
  p2<-pmin(1.0,pmax(0.0,predict(g,as.matrix(x2),s=g$lambda[length(g$lambda)],type="response")))
  p3<-pmin(1.0,pmax(0.0,predict(g,as.matrix(x3),s=g$lambda[length(g$lambda)],type="response")))
  print(mse(y2,p2))
  return(p3)
}

## pretty bulky this way, but it's very simple to get things up and running.
getGbmByMM<-function(x1,x2,x3,raw1,raw2,threshold,trees=100,depth=5,minObs=30,shrink=0.1){
  ## x1: x values for the full training set
  ## x2: x values for the holdout set (for which we have answers)
  ## x3: x values for the prediction set (generally that for which we do not have answers)
  ## raw1: target for full training: rain, in mm; called raw because this will be converted to binary
  ## raw2: target for the holdout set: also rain, in mm
  ## threshold: which rain "bucket" we are predicted: 0 = p(rain<=0mm); 1 = p(rain<=1mm); etc.
  ## trees, depth, minObs,shrink: passthrough GBM parameters
  ##
  ## returns: prediction vector, bound between 0.0 and 1.0
  ##
  ## additional feedback: prints the holdout score, mse using raw2 and predictions given x2

  library(gbm)
  library(Metrics) ##can easily remove this dependency by using the simple MSE equation
  y1<-ifelse(raw1<=threshold,1,0)
  y2<-ifelse(raw2<=threshold,1,0)

  G<-gbm.fit(x1,y1,distribution="bernoulli",n.trees=trees, interaction.depth=depth,n.minobsinnode=minObs,shrinkage=shrink)
  p2<-pmin(1.0,pmax(0.0,predict(G,x2,n.trees=trees,type="response")))
  p3<-pmin(1.0,pmax(0.0,predict(G,x3,n.trees=trees,type="response")))
  print(summary(G,plotit=FALSE))
  print(mse(y2,p2))
  return(p3)
}

getH2OGbmByMM<-function(h2oInst,x1,x2,x3,xCols,threshold,trees=100,depth=5,minObs=20,shrink=0.1,printImportance=0,printGbm=TRUE){
  ## x1: parsed H2O object for the full training set
  ## x2: parsed H2O object for the holdout set (for which we have answers)
  ## x3: parsed H2O object for the prediction set (generally that for which we do not have answers)
  ## threshold: which rain "bucket" we are predicted: 0 = p(rain<=0mm); 1 = p(rain<=1mm); etc.
  ## trees, depth,shrink: passthrough GBM parameters
  ##
  ## returns: prediction vector, bound between 0.0 and 1.0
  ##
  ## additional feedback: prints the holdout score, mse using raw2 and predictions given x2 & importance (if desired)

  library(Metrics) ##can easily remove this dependency by using the simple MSE equation

  ## convert the yBinary column into the target, specific to the threshold value passed in
  train$yBinary<-ifelse(train$Expected<=threshold,1,0)
  hold$yBinary<-ifelse(hold$Expected<=threshold,1,0)

  ## train a gbm model, then use it to predict the passed in holdout (x2) and prediction sets (x3)
  my.gbm <- h2o.gbm(x=xCols,y="yBinary",distribution="bernoulli",data=x1,key="removeWhenDone.hex",n.trees=trees,interaction.depth=depth,shrinkage=shrink,n.minobsinnode=minObs,importance=T)
  holdVals <- h2o.predict(my.gbm,x2)   ## make predictions
  returnObj<-h2o.predict(my.gbm,x3)
  returnVals<-returnObj[,3]

  ## print mse and importance values if requested
  print(mse(hold$yBinary,holdVals[,3]))
  if(printImportance<0){print(my.gbm@model$varimp)}
  if(printImportance>0){print(my.gbm@model$varimp[printImportance,])}

  ## clean up unneeded cache objects
  h2o.rm(h2oInst,"removeWhenDone.hex")
  return(returnVals)
}

getH2ODeepLearningByMM<-function(h2oInst,x1,x2,x3,xCols,threshold,
	hidden=c(200,200),epochs=1,activation="RectifierWithDropout", classification=TRUE, ...){
  ## x1: parsed H2O object for the full training set
  ## x2: parsed H2O object for the holdout set (for which we have answers)
  ## x3: parsed H2O object for the prediction set (generally that for which we do not have answers)
  ## threshold: which rain "bucket" we are predicted: 0 = p(rain<=0mm); 1 = p(rain<=1mm); etc.
  ## remaining: passthrough to h2o.deeplearning
  ##
  ## returns: prediction vector, bound between 0.0 and 1.0
  ##
  ## additional feedback: prints the holdout score, mse using raw2 and predictions given x2 & importance (if desired)

  library(Metrics) ##can easily remove this dependency by using the simple MSE equation
  slotNum<-ifelse(classification==TRUE,3,1)

  ## convert the yBinary column into the target, specific to the threshold value passed in
  train$yBinary<-ifelse(train$Expected<=threshold,1,0)
  hold$yBinary<-ifelse(hold$Expected<=threshold,1,0)

  ## train a gbm model, then use it to predict the passed in holdout (x2) and prediction sets (x3)
  my.deepL <- h2o.deeplearning(x=xCols,y="yBinary",data=x1,key="removeWhenDone.hex",
	activation=activation,hidden=hidden,epochs=epochs, classification=classification, ...)
  holdVals <- h2o.predict(my.deepL,x2)   ## make predictions
  returnObj<-h2o.predict(my.deepL,x3)
  returnVals<-returnObj[,slotNum]

  ## print mse and importance values if requested
  print(mse(hold$yBinary,holdVals[,slotNum]))

  #if(printImportance<0){print(my.gbm@model$varimp)}
  #if(printImportance>0){print(my.gbm@model$varimp[printImportance,])}

  ## clean up unneeded cache objects
  h2o.rm(h2oInst,"removeWhenDone.hex")
  return(returnVals)
}

clearh2o<-function(h2oInst){
h2o.rm(h2oInst, grep(pattern = "GBMPredict", x = h2o.ls(h2oInst)$Key, value = TRUE))
h2o.rm(h2oInst, grep(pattern = "Last.value", x = h2o.ls(h2oInst)$Key, value = TRUE))
}

### Utility functions to help look at the data
roundTexttoText<-function(x,decimals=1){
 if(as.character(x) %in% c("-",""," ","NaN","nan", "-99901","-99902","-99903","999","-99901.0","-99902.0","-99903.0","-99900","-99900.0")){return("")}
 else{return(round(as.numeric(as.character(x)),decimals))}
}

getDfFromRaw<-function(i,train){
### This will show the raw data for a particular hourly reading in an easy to read format.
### It merges the vectors, discards garbage readings, and formats the output to try and make it concise
### Currently it requires the vectors to exist, but it would be easy to make this work from the raw data
###
### Usage: getDfFromRaw(8,train), which will show you the 13 radar readings for the 8th record
### it can take a second or two to go through all the parsing
df<-as.data.frame(cbind(rep(train[i,Expected],length(train$TimeToEnd[[i]])),
unlist(lapply(lapply(as.character(train$TimeToEnd),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$DistanceToRadar),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$RadarQualityIndex),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$HydrometeorType),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$HybridScan),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$Reflectivity),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$ReflectivityQC),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$RR1),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$RR2),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$RR3),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$Zdr),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$MassWeightedMean),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$MassWeightedSD),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$Composite),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$LogWaterVolume),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x))),
unlist(lapply(lapply(as.character(train$Velocity),function(x) as.numeric(strsplit(x," ")[[1]]))[[i]],function(x) roundTexttoText(x)))
))
colnames(df)<-c("y","Tm","Dist","Qual","Type","HybSc","Refl","RefQc","RR1","RR2","RR3","Zdr","MWM","MWsd","Cmp","LWV","V")
return(df)
}


getDfFromVectors<-function(i){
### This will show the raw data for a particular hourly reading.
### It merges the vectors, discards garbage readings, and formats the output to try and make it concise
### This version is faster to use if you have the vectors pre-processed in the names provided by our pre-processing code
###
### Usage: getDfFromVectors(8), which will show you the 13 radar readings for the 8th record
df<-as.data.frame(cbind(rep(train[i,Expected],length(train.TimeToEnd[[i]])),
train.TimeToEnd[[i]],
unlist(lapply(train.DistanceToRadar[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.RadarQualityIndex[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.HydrometeorType[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.HybridScan[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.Reflectivity[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.ReflectivityQC[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.RR1[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.RR2[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.RR3[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.Zdr[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.MassWeightedMean[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.MassWeightedSD[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.Composite[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.LogWaterVolume[[i]],function(x) roundTexttoText(x))),
unlist(lapply(train.Velocity[[i]],function(x) roundTexttoText(x)))
))
colnames(df)<-c("y","Tm","Dist","Qual","Type","HybSc","Refl","RefQc","RR1","RR2","RR3","Zdr","MWM","MWsd","Cmp","LWV","V")
return(df)
}
