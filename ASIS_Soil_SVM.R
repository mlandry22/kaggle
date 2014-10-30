## this code borrows the main structure from the forum post regarding beating the benchmark
## additions to it are mainly: using two layers, updating cost parameters, capping low/high values
##  trimming the range for some targets, and using the [incorrectly calculated] derivatives.

library(e1071)
## this is not the true derivative, but it took a while before I caught it, and didn't fix it
## it is the one-column difference
deriv<-function(a){b<-c(0,a[1:length(a)-1]); a[1]<-0; return(a-b)}

train <- read.csv("./training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("./sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
submission <- test[,1]
header <- test[,1]
train[,1]<-NULL
test[,1]<-NULL
labels <- train[,c("Ca","P","pH","SOC","Sand")]

## running these in loops is not good R programming, but doing it once and exporting to .Rdata, it was simple enough to move on
dTrain<-train[,1:3578]*0
dTest<-test[,1:3578]*0
for(i in 1:nrow(dTrain)){dTrain[i,]<-deriv(t(train[i,1:3578])); if(i%%30==1){print(i)}}
for(i in 1:nrow(dTest)){dTest[i,]<-deriv(t(test[i,1:3578])); if(i%%30==1){print(i)}}
d2Train<-train[,1:3578]*0
d2Test<-test[,1:3578]*0
for(i in 1:nrow(d2Train)){d2Train[i,]<-deriv(t(dTrain[i,1:3578])); if(i%%30==1){print(i)}}
for(i in 1:nrow(d2Test)){d2Test[i,]<-deriv(t(dTest[i,1:3578])); if(i%%30==1){print(i)}}

## 10-fold cross-validation, but applied in continuous chunks to help keep grid sections together
##  intro model applied just to infrared columns in the last ~35%
cuts<-rep(0,11)
for(i in 1:10){cuts[i+1]<-round(nrow(train)*i*0.1,0)}
getCaHoldout<-function(col){
	for(fold in 1:10){
		idx<-rep(TRUE,nrow(train))
		idx[(cuts[fold]+1):cuts[fold+1]]<-FALSE
		trainLocal<-cbind(train[idx,2200:3578],dTrain[idx,2200:3578])
		testLocal<-cbind(train[idx==FALSE,2200:3578],dTrain[idx==FALSE,2200:3578])
		#train$CaEstimate<-round(labels$Ca[idx],1)
		#test$CaEstimate<-round(labels$Ca[idx==FALSE],1)

		svmCa <- svm(trainLocal,labels[idx,col],cost=10000,scale=FALSE)
		if(fold==1){p <- predict(svmCa,newdata=testLocal); h <- predict(svmCa,newdata=cbind(test[,2200:3578],dTest[,2200:3578]))}
		if(fold>1){p<-c(p,predict(svmCa,newdata=testLocal)); h<-cbind(h,predict(svmCa,newdata=cbind(test[,2200:3578],dTest[,2200:3578])))}
	}
    return(c(p,rowMeans(h)))
}
reduce1<-getCaHoldout(1)
reduce2<-getCaHoldout(2)
reduce3<-getCaHoldout(3)
reduce4<-getCaHoldout(4)
reduce5<-getCaHoldout(5)

##create separate sets for each subproblem
##	Ca: derivatives appear not to help; having more data appears to help, despite significant part of spectrum
##	P:	use as much as possible, lenght+derivatives; likely should use holdout predictions from the other elements as well
##	pH:	tough to improve; do not add derivatives
##	SOC:	no derivatives; local points help vs. all
##	Sand:	derivatives help; local points help

#make column usage specific to each target
#align with ordering of labels/targets: labels <- train[,c("Ca","P","pH","SOC","Sand")]
starts<-c(1,1,2200,2200,2300)
ends<-rep(3578,5)
derivStarts<-c(2,2200,2,2,2300)
derivEnds<-c(3,3578,3,3,3578)
#deriv2Starts<-c(2,2,2,2,2900)
#deriv2Ends<-c(3,3,3,3,3250)

##add back in the initial predictions from all five models, to all five models; e.g. Ca can see P, pH, SOC, and Sand predictions
i<-1
svmTrains<-list(cbind(train[,starts[i]:ends[i]],dTrain[,derivStarts[i]:derivEnds[i]],reduce1[1:nrow(train)]
	,reduce2[1:nrow(train)],reduce3[1:nrow(train)],reduce4[1:nrow(train)],reduce5[1:nrow(train)]))
svmTests<-list(cbind(test[,starts[i]:ends[i]],dTest[,derivStarts[i]:derivEnds[i]],reduce1[(nrow(train)+1):length(reduce1)]
	,reduce2[(nrow(train)+1):length(reduce2)],reduce3[(nrow(train)+1):length(reduce3)]
	,reduce4[(nrow(train)+1):length(reduce4)],reduce5[(nrow(train)+1):length(reduce5)]
	))
colnames(svmTrains[[i]])[(ncol(svmTrains[[i]])-4):ncol(svmTrains[[i]])]<-c("CaModel","PModel","pHModel","SOCModel","SandModel")
colnames(svmTests[[i]])[(ncol(svmTests[[i]])-4):ncol(svmTests[[i]])]<-c("CaModel","PModel","pHModel","SOCModel","SandModel")
for(i in 2:5){
	svmTrains<-c(svmTrains,list(cbind(train[,starts[i]:ends[i]],dTrain[,derivStarts[i]:derivEnds[i]],reduce1[1:nrow(train)]
		,reduce2[1:nrow(train)],reduce3[1:nrow(train)],reduce4[1:nrow(train)],reduce5[1:nrow(train)])))
	svmTests<-c(svmTests,list(cbind(test[,starts[i]:ends[i]],dTest[,derivStarts[i]:derivEnds[i]],reduce1[(nrow(train)+1):length(reduce1)]
		,reduce2[(nrow(train)+1):length(reduce2)],reduce3[(nrow(train)+1):length(reduce3)]
		,reduce4[(nrow(train)+1):length(reduce4)],reduce5[(nrow(train)+1):length(reduce5)]
		)))
	colnames(svmTrains[[i]])[(ncol(svmTrains[[i]])-4):ncol(svmTrains[[i]])]<-c("CaModel","PModel","pHModel","SOCModel","SandModel")
	colnames(svmTests[[i]])[(ncol(svmTests[[i]])-4):ncol(svmTests[[i]])]<-c("CaModel","PModel","pHModel","SOCModel","SandModel")
	}

##last-second add of Depth as binary column
isTopTrain<-as.data.frame(ifelse(train$Depth=="Topsoil",1,0))
isTopTest<-as.data.frame(ifelse(test$Depth=="Topsoil",1,0))
colnames(isTopTrain)<-"isTopsoil"
colnames(isTopTest)<-"isTopsoil"
## separate costs per SVM; these were tuned using cross-validation and roughly grid search
## adjusting gamma was also suggested by the tuning, but it was incorrectly applied when I used it and I scaled back to just cost
costs<-c(15000,15000,15000,10000,20000)

AllSvms <- lapply(1:ncol(labels),function(i){svm(cbind(svmTrains[[i]],isTopTrain),labels[,i],cost=costs[i],scale=FALSE)})
predictions1 <- sapply(1:ncol(labels),function(i){predict(AllSvms[[i]],newdata=cbind(svmTests[[i]],isTopTest))})
				
predictions<-as.data.frame(unlist(predictions1))
colnames(predictions) <- c("Ca","P","pH","SOC","Sand")

## truncate final predictions to be within range of training data; this helped considerably in early modeling 
predictions[,1]<-pmax(pmin(predictions[,1],max(labels$Ca)),min(labels$Ca))
predictions[,2]<-pmax(pmin(predictions[,2],1),min(labels$P))
predictions[,3]<-pmax(pmin(predictions[,3],max(labels$pH)),min(labels$pH))
predictions[,4]<-pmax(pmin(predictions[,4],max(labels$SOC)),min(labels$SOC))
predictions[,5]<-pmax(pmin(predictions[,5],max(labels$Sand)),min(labels$Sand))

submission <- cbind(PIDN=header,predictions)
write.csv(submission,"submission_20141021a.csv",row.names=FALSE,quote=FALSE)
