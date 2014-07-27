library(Metrics)	##load evaluation package
setwd("C:/Users/Mark_Landry/Documents/K/dozer/")
##Done in advance to speed up loading of data set
	train<-read.csv("Train.csv")
	train$saleTransform<-strptime(train$saledate,"%m/%d/%Y %H:%M")
	train<-train[order(train$saleTransform),]
	save(train,file="rTrain.Rdata")


load("rTrain.Rdata")	
xTrain<-train[(nrow(train)-149999):(nrow(train)-50000),5:ncol(train)]
xTest<-train[(nrow(train)-49999):nrow(train),5:ncol(train)]
yTrain<-train[(nrow(train)-149999):(nrow(train)-50000),2]
yTest<-train[(nrow(train)-49999):nrow(train),2]

dim(xTrain); dim(xTest)
sapply(xTrain,function(x) length(levels(x)))
	## check levels; gbm is robust, but still has a limit of 1024 per factor; for initial model, remove
	## after iterating through model, would want to go back and compress these factors to investigate 
	##	their usefulness (or other information analysis)
xTrain$saledate<-NULL; xTest$saledate<-NULL
xTrain$fiModelDesc<-NULL; xTest$fiModelDesc<-NULL
xTrain$fiBaseModel<-NULL; xTest$fiBaseModel<-NULL
xTrain$saleTransform<-NULL; xTest$saleTransform<-NULL

library(gbm)
##	Set up parameters to pass in; there are many more hyper-parameters available, but these are the most common to control
GBM_NTREES = 400
	##	400 trees in the model; can scale back later for predictions, if desired or overfitting is suspected
GBM_SHRINKAGE = 0.05
	##	shrinkage is a regularization parameter dictating how fast/aggressive the algorithm moves across the loss gradient
	##	0.05 is somewhat aggressive; default is 0.001, values below 0.1 tend to produce good results
	##		decreasing shrinkage generally improves results, but requires more trees, so the two should be adjusted in tandem
GBM_DEPTH = 4
	##	depth 4 means each tree will evaluate four decisions; 
	##		will always yield [3*depth + 1] nodes and [2*depth + 1] terminal nodes (depth 4 = 9) 
	##		because each decision yields 3 nodes, but each decision will come from a prior node
GBM_MINOBS = 30
	##	regularization parameter to dictate how many observations must be present to yield a terminal node
	##	higher number means more conservative fit; 30 is fairly high, but good for exploratory fits; default is 10

##	Fit model
g<-gbm.fit(x=xTrain,y=yTrain,distribution = "gaussian",n.trees = GBM_NTREES,shrinkage = GBM_SHRINKAGE,
	interaction.depth = GBM_DEPTH,n.minobsinnode = GBM_MINOBS)
	## gbm fit; provide all remaining independent variables in xTrain; provide targets as yTrain;
	##	gaussian distribution will optimize squared loss; 

## get predictions; first on train set, then on unseen test data
tP1 <- predict.gbm(object = g,newdata = xTrain,GBM_NTREES)
hP1 <- predict.gbm(object = g,newdata = xTest,GBM_NTREES)

## compare model performance to default (overall mean)
rmse(yTrain,tP1)			##  9452.742 on data used for training
rmse(yTest,hP1)				##  9740.559 ~3% drop on unseen data; does not seem to be overfit
rmse(yTest,mean(yTrain))	## 24481.08  overall mean; cut error rate (from perfection) by 60%

## look at variables
summary(g)	## summary will plot and then show the relative influence of each variable to the entire GBM model (all trees)

## test dominant variable mean
library(sqldf)
trainProdClass<-as.data.frame(cbind(as.character(xTrain$fiProductClassDesc),yTrain))
testProdClass<-as.data.frame(cbind(as.character(xTest$fiProductClassDesc),yTest))
colnames(trainProdClass)<-c("fiProductClassDesc","y"); colnames(testProdClass)<-c("fiProductClassDesc","y")
ProdClassMeans<-sqldf("SELECT fiProductClassDesc,avg(y) avg, COUNT(*) n FROM trainProdClass GROUP BY fiProductClassDesc")
ProdClassPredictions<-sqldf("SELECT case when n > 30 then avg ELSE 31348.63 end avg 
FROM ProdClassMeans P LEFT JOIN testProdClass t ON t.fiProductClassDesc = P.fiProductClassDesc")
rmse(yTest,ProdClassPredictions$avg)	## 29082.64	? peculiar result on the fiProductClassDesc means, which seemed fairly stable and useful
										##seems to say that the primary factor alone is not helpful; full tree needed


## Investigate actual GBM model
pretty.gbm.tree(g,1)	##	show underlying model for the first decision tree
summary(xTrain[,10])	##	underlying model showed variable 9 to be first point in tree (9 with 0 index = 10th column)
g$initF					##	view what is effectively the "y intercept"
mean(yTrain)			##	equivalence shows gaussian y intercept is the mean
t(g$c.splits[1][[1]]) 	##	show whether each factor level should go left or right
plot(g,10)				##	plot fiProductClassDesc, the variable with the highest rel.inf
plot(g,3)				##	plot YearMade, continuous variable with 2nd highest rel.inf
interact.gbm(g,xTrain,c(10,3))
						##	compute H statistic to show interaction; integrates 
interact.gbm(g,xTrain,c(10,3))
						##	example of uninteresting interaction
 
