setwd("/Users/mark/Documents/rossman/models/")
library(data.table)  
library(h2o)

cat("reading the train and test data (with data.table) \n")
train <- fread("../input/train.csv",stringsAsFactors = T)
store <- fread("../input/store.csv",stringsAsFactors = T)

dim(train)     ## look at size
train          ## take a look at some example rows
store          ## examples from the store data

## Merge (i.e. join) the train set with the store data
train <- merge(train,store,by="Store")
train          ## glance at merged data

## Conver date field into a proper date format, then extract month and year
train[,Date:=as.Date(Date)]
train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train   ## make sure the calculations came out as expected

## Use H2O's random forest
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
trainHex<-as.h2o(train[year <15 | month <6,],destination_frame = "trainHex")
validHex<-as.h2o(train[year == 15 & month >= 6,],destination_frame = "validHex")
dim(trainHex); dim(validHex)

## Set up variable to use all features other than those specified here
features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","Customers"))]
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="Sales", 
                          training_frame=trainHex,
                          validation_frame=validHex,
                          model_id="introRF")

## Now let's see how it performed

## The specific competition error metric is not defined, so we will define it
rmspe<-function(actuals,predictions){return(mean(((actuals[actuals>0]-predictions[actuals>0])/actuals[actuals>0])^2)^0.5)}
## Create a data frame with the actuals and H2O predictions 
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(rfHex,validHex))[,1]))

## Score the predictions 
rmspe(judge[,1],judge[,2])

## And now take a look at the model summary
summary(rfHex)

## How good is that model?
theMean<-mean(trainHex$Sales)
theMedian<-median(as.data.frame(trainHex$Sales)[,1])
theLogMean<-expm1(mean(log1p(as.data.frame(trainHex$Sales)[,1])))
rmspe(judge[,1],rep(theMean,nrow(judge)))
rmspe(judge[,1],rep(theMedian,nrow(judge)))
rmspe(judge[,1],rep(theLogMean,nrow(judge)))

## Seems like it's great. But is it?

train[Store==1,.(Sales,Date)][order(Date),plot(Sales)]

## A lot of 0s
train[,.(.N,ZeroRate=sum(Sales==0)/.N,AvgSales=mean(Sales),AvgSalesNoZero=mean(ifelse(Sales==0,NA,Sales),na.rm=T))]

## Look at example records with 0 Sales
train[Sales==0,]

## Summarize by Open status 
train[,.(.N,mean(Sales)),Open]   ## data table syntax, SQL style: [i,j,by] = [WHERE,SELECT,GROUP BY]

## So the model is learning the Open status well, as we saw from the summary showing it as the most important column
##   In practice, this is fine, as it's an easy and obvious way to separate the data. 
##   Nobody will be impressed by our ability to separate Open/Closed stores really well.
## But there are two considerations, still. One is that it is so specific, it might be better to separate the data anyway
##   via a rule-based system. The rationale is that with anything that samples columns, it might try and fit a tree with
##   vital information. And with 17k records at exactly 0 sales, plus the intuitive concept that Closed = 0 sales, there
##   should be no advantage leaving that data in. 
##   So let's try removing it.
## Additionally, we are not scored on results where the actuals are 0. That is for math simplicity on Kaggle's side.
##   In practice, you do not want to be so leniant. If your model ignores non-zero predictions when the actual was zero
##   you will have a problem with whatever action is connected to your predictions (e.g. setting production volume)
## So for both reasons, we will remove the 0 sales and conduct all future modeling without these in the training set

train <- train[Sales > 0,]  ## We are not judged on 0 sales records in test set

## Now let's try to retrain the model and see if there is a difference.
trainHex<-as.h2o(train[year <15 | month <6,],destination_frame = "trainHex")
validHex<-as.h2o(train[year == 15 & month >= 6,],destination_frame = "validHex")
dim(trainHex); dim(validHex)
rfHex <- h2o.randomForest(x=features,y="Sales",training_frame=trainHex,validation_frame=validHex,model_id="introRF")
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(rfHex,validHex))[,1]))
rmspe(judge[,1],judge[,2])

## Great. The model is better. What next?
summary(rfHex)

## Odd that Store is less important than CompetitionDistance.
## We have chosen to solve a time series problem with decision trees, which should require heavy use
##  of the main two dimensions (store & time)
class(train$Store)

## Change integer encoding to a factor: should be no logical reason why an ID number encodes any value in the numbering scheme
train[,Store:=as.factor(as.numeric(Store))]

## Now let's try to retrain the model and see if there is a difference.
trainHex<-as.h2o(train[year <15 | month <6,],destination_frame = "trainHex")
validHex<-as.h2o(train[year == 15 & month >= 6,],destination_frame = "validHex")
dim(trainHex); dim(validHex)
rfHex <- h2o.randomForest(x=features,y="Sales",training_frame=trainHex,validation_frame=validHex,model_id="introRF",
                          nbins_cats=1115)
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(rfHex,validHex))[,1]))
rmspe(judge[,1],judge[,2])

## More improvement. What next?

## log transformation to not be as sensitive to high sales
## decent rule of thumb: 
##     if the data spans an order of magnitude, consider a log transform
train[,logSales:=log1p(Sales)]
features<-colnames(train)[!(colnames(train) %in% c("Id","Date","Sales","Customers","logSales"))]

## Run through experimentation cycle again
trainHex<-as.h2o(train[year <15 | month <6,],destination_frame = "trainHex")
validHex<-as.h2o(train[year == 15 & month >= 6,],destination_frame = "validHex")
dim(trainHex); dim(validHex)
rfHex <- h2o.randomForest(x=features,y="logSales",training_frame=trainHex,validation_frame=validHex,model_id="introRF",
                          nbins_cats=1115)
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(rfHex,validHex))[,1]))
rmspe(judge[,1],expm1(judge[,2]))  ## added expm1 term to undo the log transformation for real results

## There is a lot more experimentation that can be done with features. 
## But for the moment, we'll try some other modeling choices at this point.
## First, let's try a GBM
gbmHex <- h2o.gbm(x=features,y="logSales",training_frame=trainHex,validation_frame=validHex,model_id="introGBM",
                          nbins_cats=1115)
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(gbmHex,validHex))[,1]))
rmspe(judge[,1],expm1(judge[,2]))  ## added expm1 term to undo the log transformation for real results

## It ran faster, but is less accurate. Let's add a stochastic element to it
gbmHex <- h2o.gbm(x=features,y="logSales",training_frame=trainHex,validation_frame=validHex,model_id="introGBM",
                  nbins_cats=1115,sample_rate = 0.7,col_sample_rate = 0.7,max_depth = 10,learn_rate=0.05)
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(gbmHex,validHex))[,1]))
rmspe(judge[,1],expm1(judge[,2]))  ## added expm1 term to undo the log transformation for real results

## Still not matching RF accuracy
gbmHex <- h2o.gbm(x=features,y="logSales",training_frame=trainHex,validation_frame=validHex,model_id="introGBM",
                  nbins_cats=1115,sample_rate = 0.5,col_sample_rate = 0.5,max_depth = 15,learn_rate=0.05,ntrees = 50)
judge<-as.data.frame(cbind(as.data.frame(validHex$Sales),as.data.frame(h2o.predict(gbmHex,validHex))[,1]))
rmspe(judge[,1],expm1(judge[,2]))  ## added expm1 term to undo the log transformation for real results

## Closer. To really compare the two, we'd likely want to do a grid search here.


## When we like our model, predict on test to see if our out of sample translates reasonably well
## Note: this wasn't run at the Data Science Camp
test  <- fread("../input/test.csv",stringsAsFactors = T)
test <- merge(test,store,by="Store")
test[,Date:=as.Date(Date)]
test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]
test[,Store:=as.factor(as.numeric(Store))]
summary(test)

testHex<-as.h2o(test)
testPredictions<-expm1(as.data.frame(h2o.predict(rfHex,testHex))[,1])
summary(testPredictions)
