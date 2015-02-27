library(data.table)
library(R.matlab)
personList<-c("S02","S06","S07","S11","S12","S13","S14","S16","S17","S18","S20","S21","S22","S23","S24","S26")
sessionList<-c("01","02","03","04","05")
for(i in 1:length(personList)){
  for(j in 1:length(sessionList)){
	fileName<-paste0("train/Data_",personList[i],"_Sess",sessionList[j])
	x<-as.matrix(fread(paste0(fileName,".csv")))
	writeMat(paste0(fileName,".mat"),x=x)
  }
}

library(data.table)
library(R.matlab)
personList<-c("S01","S03","S04","S05","S08","S09","S10","S15","S19","S25")
sessionList<-c("01","02","03","04","05")
for(i in 1:length(personList)){
  for(j in 1:length(sessionList)){
	fileName<-paste0("train/Data_",personList[i],"_Sess",sessionList[j])
	x<-as.matrix(fread(paste0(fileName,".csv")))
	writeMat(paste0(fileName,".mat"),x=x)
  }
}
