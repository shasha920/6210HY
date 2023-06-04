install.packages(c("neuralnet",
                 "nnet",
                 "caret"))
library(neuralnet)
library(nnet)
library(caret)

#Table 11.2
tiny.df<-read.csv("TinyData.csv")
tiny.df$Acceptance<-factor(tiny.df$Acceptance,levels=c("dislike","like"))
#By default it uses resilient backpropagation with weight backtracking
#Please see details in Riedmiller M. and Braun H.(1993) A direct adaptive
#method for faster backpropagation learing: The RPROP algorithm
#Proceddings of the IEE International Conference on Neural Networkds
#(ICNN), pages 586-591. San Francisco.
#Stopping rules:
#1. a threshold for the partial dervivatives of error function
#set with "threshold" parameter, default 0.01
#2.maximum steps for the training of the neural network
#set with "stepmax" parameter, default 100,000
#Default output layer activation function is logistic function(see act.fct)
#Set linear.output=F to use the output layer activation function
nn<-neuralnet(Acceptance~Salt+Fat,data=tiny.df,linear.output = F, hidden = 3)
#Display weights
options(scipen=9999)
nn$weights
#Display predictions
#Note that the predicted values(similar to propensities)
#may NOT necessarily sum up to 1!
prediction(nn)
#Plot neural network
#Search for "plot.nn"
plot(nn,rep="best")

#Table 11.3
pred.prob<-predict(nn, data.frame(tiny.df$Salt,tiny.df$Fat))
pred.class<-apply(pred.prob,1,which.max)-1
pred.class<-ifelse(pred.class=="0","dislike","like")
pred.class<-factor(pred.class,levels = c("dislike","like"))
confusionMatrix(pred.class,tiny.df$Acceptance)


#Table 11.6
accidents.df<-read.csv("accidentsnn.csv")
accidents.df$MAX_SEV_IR<-factor(accidents.df$MAX_SEV_IR)
#Select variables
vars=c("ALCHL_I","PROFIL_I_R","VEH_INVL")
#Create dummies for SUR_COND and MAX_SEV_IR
accidents.dummy.df<-cbind(accidents.df[,vars],
                          data.frame(class.ind(accidents.df$SUR_COND)),
                          data.frame(class.ind(accidents.df$MAX_SEV_IR)))
names(accidents.dummy.df)=c(vars,paste("SUR_COND_",c(1,2,3,4,9),sep=""),
                            paste("MAX_SEV_IR_",c(0,1,2),sep = ""))
#Partition the data
set.seed(2)
train.index=sample(row.names(accidents.dummy.df),dim(accidents.dummy.df)[1]*0.6)
valid.index=setdiff(row.names(accidents.dummy.df),train.index)
train.df<-accidents.dummy.df[train.index,]
valid.df<-accidents.dummy.df[valid.index,]
#Train neural network with 2 hidden nodes
#Results may be slightly different from the book
#due to randomness in neural network training
#Set seed to reproduce the same results
set.seed(10)
nn<-neuralnet(MAX_SEV_IR_0+MAX_SEV_IR_1+MAX_SEV_IR_2~ALCHL_I+PROFIL_I_R
              +VEH_INVL+SUR_COND_1+SUR_COND_2+SUR_COND_3+SUR_COND_4,
              data=train.df,hidden=2)
plot(nn,rep="best")
#Training results
train.pred.prob<-predict(nn,train.df[,-c(9:11)])
train.pred.class<-apply(train.pred.prob,1,which.max)-1
train.pred.class<-factor(train.pred.class)
confusionMatrix(train.pred.class,accidents.df[train.index,]$MAX_SEV_IR)
#Validation results
valid.pred.prob<-predict(nn,valid.df[,-c(9:11)])
valid.pred.class<-apply(valid.pred.prob,1,which.max)-1
valid.pred.class<-factor(valid.pred.class)
confusionMatrix(valid.pred.class,accidents.df[valid.index,]$MAX_SEV_IR)