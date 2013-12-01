train<-read.csv("train.csv")
train<-train[train$num_votes!=327,]                ##remove large outlier
train<-train[train$num_views<1500,]                ##remove large outlier
train$created_time<-strptime(train$created_time,"%Y-%m-%d %H:%M:%S")
#train<-train[as.Date(train$created_time)>as.Date("2012-10-14"),]
train<-train[as.Date(train$created_time)>as.Date("2013-01-31"),]

test<-read.csv("test.csv")
test$created_time<-strptime(test$created_time,"%Y-%m-%d %H:%M:%S")

#########################
##### Functions #########
triRmsle<-function(p1,a1,p2,a2,p3,a3){
 require(Metrics)
 a<-rmsle(rbind(p1,p2,p3),rbind(a1,a2,a3))
 return(a)
}
quickScalarRmsle<-function(df,scalars){return(triRmsle(df[,1]*scalars[,1],df[,1],df[,2]*scalars[,1],df[,2],df[,3]*scalars[,1],df[,3]))}
##usage: quickScalarRmsle(targets,cbind(targets[,1]*0,targets[,2]*0,targets[,3]*0)) ##0.7330985

getGbmFit<-function(x,y,t){require(gbm); GBM_NTREES = t; GBM_SHRINKAGE = 0.05; GBM_DEPTH = 4; GBM_MINOBS = 30; 
        return(gbm.fit(x = x,y = y,distribution = "gaussian",n.trees = GBM_NTREES,shrinkage = GBM_SHRINKAGE,interaction.depth = GBM_DEPTH,n.minobsinnode = GBM_MINOBS,verbose = FALSE))}

getDistributionTable<-function(singleColumnDF,n){
        require(sqldf)
        colnames(singleColumnDF)<-"x"
        out<-as.data.frame(sqldf(paste("SELECT x FROM singleColumnDF GROUP BY x ORDER BY COUNT(*) DESC LIMIT",n)))
        return(out)
}

binarizeColumn <- function(data,valListDF,colNum,remove=TRUE){
        for(i in 1:nrow(valListDF)){
                newCol<-ifelse(data[,colNum]==valListDF[i,1],1,0)
                data<-as.data.frame(cbind(data,newCol))
                colnames(data)[ncol(data)]<-as.character(valListDF[i,1])
                }
        if(remove) {data[,colNum]<-NULL}
        return(data)
}        

###### End of functions #######        
###############################


x<-test[,c(1,1,1,1:8)]
colnames(x)<-colnames(train)[c(6,7,8,1:5,9:11)]
x<-as.data.frame(rbind(train[,c(6,7,8,1:5,9:11)],x))
x<-as.data.frame(cbind(seq(1:nrow(x)),x))
colnames(x)[1]<-"sortNum"

a1<-1; a2<-nrow(train); b1<-nrow(train)+1; b2<-nrow(x)
a1;a2;b1;b2

city<-as.factor(ifelse(x$longitude<(-100),"Oakland",ifelse(x$longitude<(-82),"Chicago",ifelse(x$longitude<(-75),"Richmond","New Haven"))))
descLength<-round(log(nchar(as.character(x$description))+1),0)
descSummary<-round(log(nchar(as.character(x$summary))+1),0)
latlong2_str<-as.factor(paste(round(x$latitude/2,2)*2,round(x$longitude/2,2)*2,sep='_'))
hrOfDay<-as.factor(substr(x$created_time,12,13))
dayOfWeek <- as.factor(weekdays(as.Date(x$created_time)))
l10sum<-as.factor(tolower(substr(gsub(" ","",gsub("[[:punct:]]","",x$summary)),1,10)))
 ##left_10_desc<-as.factor(tolower(substr(gsub(" ","",gsub("[[:punct:]]","",x$description)),1,10)))
x$latitude<-NULL; x$longitude<-NULL

x<-as.data.frame(cbind(x,city,descLength,descSummary,latlong2_str,hrOfDay,dayOfWeek,l10sum))

levels(x$source)<-c(levels(x$source),"n/a"); x$source[is.na(x$source)]<-"n/a"
levels(x$tag_type)<-c(levels(x$tag_type),"n/a"); x$tag_type[is.na(x$tag_type)]<-"n/a"

## binarization
distL10Summ<-getDistributionTable(as.data.frame(x$l10sum),50)
x<-binarizeColumn(x,distL10Summ,17)

distlatlong2_str<-getDistributionTable(as.data.frame(x$latlong2_str),50)
x<-binarizeColumn(x,distlatlong2_str,14)

distdayOfWeek<-getDistributionTable(as.data.frame(x$dayOfWeek),7)
x<-binarizeColumn(x,distdayOfWeek,15)

disthrOfDay<-getDistributionTable(as.data.frame(x$hrOfDay),24)
x<-binarizeColumn(x,disthrOfDay,14)

distdescSummary<-getDistributionTable(as.data.frame(x$descSummary),6)
x<-binarizeColumn(x,distdescSummary,13)

distdescLength<-getDistributionTable(as.data.frame(x$descLength),6)
x<-binarizeColumn(x,distdescLength,12)

distcity<-getDistributionTable(as.data.frame(x$city),4)
x<-binarizeColumn(x,distcity,11)

disttag_type<-getDistributionTable(as.data.frame(x$tag_type),20)
x<-binarizeColumn(x,disttag_type,10)

distsource<-getDistributionTable(as.data.frame(x$source),9)
x<-binarizeColumn(x,distsource,8)

a1<-1; a2<-nrow(train); b1<-nrow(train)+1; b2<-nrow(x)
a1;a2;b1;b2
train<-x[x$sortNum<b1,]
test<-x[x$sortNum>=b1,]
dim(train); dim(test)

trees<-100
fitVotes<-getGbmFit(train[,9:ncol(train)], log(1+train[,2]), trees)
fitComments<-getGbmFit(train[,9:ncol(train)], log(1+train[,3]), trees)
fitViews<-getGbmFit(train[,9:ncol(train)], log(1+train[,4]), trees)

cvFullVotes<-predict(object = fitVotes,newdata=train[,9:ncol(train)], type="response",trees)
cvFullComments<-predict(object = fitComments,newdata=train[,9:ncol(train)], type="response",trees)
cvFullViews<-predict(object = fitViews,newdata=train[,9:ncol(train)], type="response",trees)

testVotesNS<-predict(object = fitVotes,newdata=test[,9:ncol(test)], type="response",trees)
testCommentsNS<-predict(object = fitComments,newdata=test[,9:ncol(test)], type="response",trees)
testViewsNS<-predict(object = fitViews,newdata=test[,9:ncol(test)], type="response",trees)

quickScalarRmsle(cbind(train[,2],train[,3],train[,4]),cbind(exp(cvFullVotes)-1,exp(cvFullComments)-1,exp(cvFullViews)-1))
summary(fitViews)
summary(fitVotes)
summary(fitComments)

submission<-as.data.frame(cbind(test[,5],round(exp(testViews)-1,3),round(exp(testVotes)-1,3),round(exp(testComments)-1,3)))
colnames(submission)<-c("id","num_views","num_votes","num_comments")
submission$num_views<-pmax(submission$num_views,0)
submission$num_votes<-pmax(submission$num_votes,0)
submission$num_comments<-pmax(submission$num_comments,0)
t<-gsub("[[:punct:]]", "", as.character(Sys.time()))
fn<-paste("submission_",gsub(" ","_",t),".csv",sep='')
write.table(submission,fn, append = FALSE, sep=',', row.names=FALSE, quote=FALSE, col.names=TRUE)
