install.packages("FNN")
install.packages("caret")
insatll.packages("e1071")
install.packages("ggplot2")
install.packages("lattice")
library(FNN)
library(caret)
library(e1071)
library(ggplot2)
library(lattice)

#Figure 7.1
mower.df<-read.csv("RidingMowers.csv")
mower.df[,3]<-factor(mower.df[,3],levels=c("Owner","Nonowner"))
set.seed(111)
#Partition data into training (60%) and validation (40%) sets
train.index<-sample(row.names(mower.df),dim(mower.df)[1]*0.6)
valid.index<-setdiff(row.names(mower.df),train.index)
train.df<-mower.df[train.index, ]
valid.df<-mower.df[valid.index, ]
#Create new data to be classified
new.df <- data.frame(Income = 60, Lot_Size = 20)
#Scatter plot without new household
plot(Lot_Size~Income, data=train.df,main="",xlab="Income($000s)",
     ylab="Lot Size (000s sqft)",pch=ifelse(train.df$Ownership=="Owner",19,1))
text(train.df$Income,train.df$Lot_Size,rownames(train.df),pos=4)
legend("topright",inset=0,legend=c("Owner","Nonowner"),
       pch=c(19,1),cex=0.75)
#Scatter plot with new household
plot(Lot_Size~Income,data=train.df,main="",xlab="Income ($000s)",
     ylab="Lot Size (000s sqft)",pch=ifelse(train.df$Ownership=="Owner",19,1))
text(train.df$Income,train.df$Lot_Size,rownames(train.df),pos=4)
points(new.df$Income,new.df$lot_size,pch=4)
legend("topright",inset=0,legend=c("Owner","Nonowner","New Household"),
       pch=c(19,1,4),cex=0.75)

#Table 7.2
#Normalize variables using mean and standard deviation extracted from training set only
norm.train.df<-train.df
norm.valid.df<-valid.df
norm.mower.df<-mower.df
norm.new.df<-new.df
norm.values<-preProcess(train.df[,1:2],method=c("center","scale"))
norm.train.df[,1:2]<-predict(norm.values,train.df[,1:2])
norm.valid.df[,1:2]<-predict(norm.values,valid.df[,1:2])
norm.mower.df[,1:2]<-predict(norm.values,mower.df[,1:2])
norm.new.df<-predict(norm.values,new.df)
#Run KNN
knn.pred.new<-knn(train=norm.train.df[,1:2],test=norm.new.df,
                  cl=norm.train.df[,3],k=3)
#Show the identified 3 nearest neighbors of the new record
row.names(norm.train.df)[attr(knn.pred.new,"nn.index")]
#Show the classification of the new record
attr(knn.pred.new,"levels")

#Table 7.3
#Initialize a series of k values we want to try
accuracy.df<- data.frame(k=seq(1,14,1),accuracy=rep(0,14))
#Compute the KNN for different k on validation set
for(i in 1:dim(accuracy.df)[1]){
  knn.pred.valid<- knn(norm.train.df[,1:2],norm.valid.df[,1:2],
                      cl=norm.train.df[,3],k=accuracy.df$k[i])
  knn.pred.valid<- factor(knn.pred.valid,levels=levels(mower.df[,3]))
  accuracy.df$accuracy[i]<-confusionMatrix(knn.pred.valid,
                                           norm.valid.df[,3])$overall[1]
}
#Print the results
accuracy.df

#Table 7.4
#Run KNN with the best kvalue
knn.pred.new <- knn(norm.mower.df[,1:2], norm.new.df,cl=norm.mower.df[,3],k=5)
#Show the identified 5 nearest neighbors of the new record
row.names(norm.mower.df)[attr(knn.pred.new,"nn.index")]
#Show the classification of the new record
attr(knn.pred.new,"levels")