start<-Sys.time()
#setwd("directory where the data files are located, if not getwd()")

## read in a fixed number of rows (increase for improved accuracy)
## you can use read.csv in place of data.table; it's just much slower
library(data.table)	
train<-fread("train_2013.csv",nrows=250000,select=c("RR1","Expected"))

## parse the readings inside each RR1 value, which are space-delimited
rr1.mean<-unlist(lapply(train[,RR1], function(x) mean(pmax(0,pmin(70,as.numeric(strsplit(x," ")[[1]]))))))
rr1.mean[is.na(rr1.mean)]<-0

## look at the data
## notice most observations are 0, and after that most between 0 and 0.5
## so using typical quantiles or breaks will merely subdivide a really small range of measurements
## So instead, we break off the most popular buckets, and then divide the remainder of the RR1 distribution
table(round(rr1.mean,1))
breaks<-c(-Inf,0,quantile(rr1.mean[rr1.mean>0.5],c(0:8)/8))
names(breaks)<-NULL
n<-length(breaks)-1

## create a matrix to store predictions for key points along the RR1 distribution
observations<-matrix(,n,10)

## now override the default 100% with empirical values
for(i in 1:n){	
	## for the various cutpoints (breaks) in the RR1 distribution, measure the frequency
	##	of the bottom 10 levels of the target value (0mm,1mm,2mm,...9mm)
	observations[i,1:10]<-hist(
		pmin(70,pmax(0,train[,Expected][rr1.mean>breaks[i]&rr1.mean<=breaks[i+1]])),
		plot=FALSE,
		breaks=c(0:(n-1),70)
		)$density
}

## convert histogram to cumulative distribution
## this ensures we have non-decreasing predictions as well (requirement)
for(i in 2:n){observations[,i]<-observations[,(i-1)]+observations[,i]}

## look over the values again
## looks a little peculiar in that some higher ranges have lower outcomes, but it is empirical
observations

## now construct a prediction by using the matrix as a lookup table to all 70 predictions
##  i.e. we'll find the RR1 value for every record, figure out which row in our small prediction table
##  it corresponds to, and use that entire row of the matrix as our prediction "vector"

test<-fread("test_2014.csv",select=c("Id","RR1"))

## parse the readings inside each RR1 value, which are space-delimited
rr1.mean<-unlist(lapply(test[,RR1], function(x) mean(pmax(0,pmin(70,as.numeric(strsplit(x," ")[[1]]))))))
rr1.mean[is.na(rr1.mean)]<-0

## seed the predictions with 1, which means it will rain <= each column 100% of the time
predictions<-as.data.frame(cbind(test$Id,matrix(1,nrow=nrow(test),ncol=70)))
colnames(predictions)<-c("Id",paste0("Predicted",(seq(1:70)-1)))

## override the 100% values with our table lookup; want to improve this? smooth the edge
for(i in 1:n){predictions[rr1.mean>breaks[i]&rr1.mean<=breaks[i+1],2:(n+1)]<-as.data.frame(t(observations[i,1:n]))}

## output predictions; outputs as 188MB, but compresses to 6.8MB (lot of 1's)
options("scipen"=100, "digits"=8)
write.table(predictions,"histogram_benchmark.csv",quote = FALSE, sep = ",",row.names=FALSE)

##how long did it take (3.3 minutes on a Windows i5 with 6GB memory and slow HDD)
stop<-Sys.time()
stop-start
