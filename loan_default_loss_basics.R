## Posting the basics of our loss model. 
## R objects were often saved to speed the process up, so currently the pieces are being put in, 
##  without any cohesion to get the content in one place.

gbmCv<-function(xTrain,yTrain,n=10,trees=500,shrink=0.05,depth=6,minobs=30,dist="laplace"){
require(gbm,Metrics)
t<-seq(1:nrow(xTrain))
for(i in (1:n)){
    xH<-xTrain[t%%n==(i-1),]
    xT<-xTrain[t%%n!=(i-1),]
    yH<-yTrain[t%%n==(i-1)]
    yT<-yTrain[t%%n!=(i-1)]
    GBM_model <- gbm.fit(x=xT,y=yT,distribution=dist,n.trees=trees,shrinkage=shrink,interaction.depth=depth,n.minobsinnode=minobs,verbose=FALSE)
    pT<-predict.gbm(object=GBM_model,newdata=xT,trees)
    pH<-predict.gbm(object=GBM_model,newdata=xH,trees)
    if(i==1){cvDf<-as.data.frame(cbind(i,mae(yT,pT),mae(yH,pH)))}
    else{cvDf<-rbind(cvDf,as.data.frame(cbind(i,mae(yT,pT),mae(yH,pH))))}
    }
return(cvDf)
}

gbmDualCv<-function(xTrain,yTrain,n=10,trees=500,shrink=0.05,depth=6,minobs=30){
require(gbm,Metrics)
t<-seq(1:nrow(final_x))
for(i in (1:n)){
    xH<-final_x[t%%n==(i-1),]
    xT<-final_x[t%%n!=(i-1),]
    yH<-y[t%%n==(i-1)]
    yT<-y[t%%n!=(i-1)]
    GBM_laplace <- gbm.fit(x=xT,y=((yT)^0.5),distribution="laplace",n.trees=trees,shrinkage=0.05,interaction.depth=depth,n.minobsinnode=30,verbose=FALSE)
    GBM_gauss <- gbm.fit(x=xT,y=(yT^0.5),distribution="gaussian",n.trees=trees,shrinkage=0.05,interaction.depth=depth,n.minobsinnode=30,verbose=FALSE)
    pT<-predict.gbm(object=GBM_laplace,newdata=xT,trees)
    pH<-predict.gbm(object=GBM_laplace,newdata=xH,trees)
    pTg<-predict.gbm(object=GBM_gauss,newdata=xT,trees)
    pHg<-predict.gbm(object=GBM_gauss,newdata=xH,trees)

    if(i==1){cvDf<-as.data.frame(cbind(i,
        mae(yT,pmax(0,pT^2)),    
        mae(yH,pmax(0,pH^2)),   
        mae(yT,pmax(0,pTg^2)),   
        mae(yH,pmax(0,pHg^2)),   
        mae(yH,pmax(0,(pH^2+pHg^2)/2))))}    
    else{cvDf<-rbind(cvDf,as.data.frame(cbind(i,
        mae(yT,pmax(0,pT^2)),    
        mae(yH,pmax(0,pH^2)),   
        mae(yT,pmax(0,pTg^2)),   
        mae(yH,pmax(0,pHg^2)),   
        mae(yH,pmax(0,(pH^2+pHg^2)/2))))}    
    }
return(cvDf)
}  

##Find initial features for GBM using those most highly correlated with loss
##  this is overkill to get the full correlation matrix just to find the variables
##  correlated with the target, but much time was spent looking through the rest of the matrix
##  though it was never used. 
topCorrelatedFeatures<-function(train,threshold=0.25){
    require(Hmisc)
    rc<-rcorr(as.matrix(train))
    rcdf<-as.data.frame(rc$r)
    r2<-as.data.frame(t(rcdf[1:2,]))
    r3<-r2[abs(r2$loss)>0.25,]
    attach(r3)
    out<-r3[order(-loss),]
    detach(r3)
    return(out)
}    

##Impute values for algorithms that require it, using caret's imputation method
require(caret); require(Metrics)
pp<-preProcess(final_x,method="medianImpute")   ##very fast
x2<-predict(pp,final_x)
x3<-predict(pp,test[,2:ncol(test)])


## Main driver section
dirs<-read.csv("project_locations.csv")
setwd(as.character(dirs[1,2]))
rm(dirs)

##to do: replace this with code to create the data set
load("final_x.Rdata")
load("final_y.Rdata")
load("test_final_with_id.Rdata")  ##test

## example CV test
df<-gbmDualCv(final_x,y,10,1000,0.05,10,30); sapply(df,mean)

## run final data
fp<-gbmFull(final_x,y,test,10,700,0.05,7,30,"laplace")
write.table(fp,"gbm_20140227b.csv",append = FALSE, sep=',', row.names=FALSE, quote=TRUE, col.names=TRUE)
