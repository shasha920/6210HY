install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("e1071")
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

#Figure 9.8
mower.df<-read.csv("RidingMowers.csv")
mower.df[,3]<-factor(mower.df[,3],levels=c("Owner","Nonowner"))
#Build classification tree
mower.default.ct<-rpart(Ownership~.,data=mower.df,method = "class",
                        control = rpart.control(minsplit = 1))
prp(mower.default.ct,type=1,extra = 1,under=TRUE,split.font=2,
    under.font = 1,nn.font=3,varlen=-10,
    box.col = ifelse(mower.default.ct$frame$var=="<leaf>","gray","white"))

#Figure 9.9,9.10 and Table 9.3
bank.df<-read.csv("UniversalBank.csv")
bank.df<-bank.df[,-c(1,5)]

#Partition data into training(60%) and validation(40%) sets
set.seed(1)
bank.train.index<-sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)
bank.train.df<-bank.df[bank.train.index,]
bank.valid.df<-bank.df[-bank.train.index,]

#Build the default (best_pruned) classification tree
bank.default.ct<-rpart(Personal.Loan~.,data=bank.train.df,method = "class",
                        control = rpart.control(xval=10))

#Plot tree
prp(bank.default.ct,type = 1,extra=1,under=TRUE,split.font = 2,
    under.font = 1,nn.font = 3,varlen = -10,
    box.col = ifelse(bank.default.ct$frame$var=="<leaf>","gray","pink"))

#Performance evaluation on training set
bank.default.ct.pred.train<-predict(bank.default.ct,bank.train.df,type="class")
confusionMatrix(bank.default.ct.pred.train,as.factor(bank.train.df$Personal.Loan))

#Performance evaluation on validation set
bank.default.ct.pred.valid<-predict(bank.default.ct,bank.valid.df,type="class")
confusionMatrix(bank.default.ct.pred.valid,as.factor(bank.valid.df$Personal.Loan))

#Build a fully grown classification tree
bank.full.ct<-rpart(Personal.Loan~., data=bank.train.df,method = "class",
                    control = rpart.control(minsplit = 1,cp=0))

#Plot tree
prp(bank.full.ct,type=1,extra=1,under=TRUE,split.font = 2,
    under.font = 1,nn.font = 3,varlen = -10,
    box.col = ifelse(bank.full.ct$frame$var=="<leaf>","gray","pink"))

#Performance evaluation on training set
bank.full.ct.pred.train<-predict(bank.full.ct,bank.train.df,type = "class")
confusionMatrix(bank.full.ct.pred.train,as.factor(bank.train.df$Personal.Loan))

#Performance evaluation on validation set
bank.full.ct.pred.valid<-predict(bank.full.ct,bank.valid.df,type = "class")
confusionMatrix(bank.full.ct.pred.valid,as.factor(bank.valid.df$Personal.Loan))

#Table 9.4
#Perform cross validation within training dataset and record Compexity Parameters(cp)
bank.ct<-rpart(Personal.Loan~., data=bank.train.df,method = "class",
               control = rpart.control(cp=0.00001,minsplit = 1,xval=10))
#rel error (relative error): training erro
#xerror (relative error):validation error
#xstd (relative stdev):validation stdev
#Each row represents a different level of tree
#which is the best tree with the same level
printcp(bank.ct)
#The plot shows the change of xerror with CP
#The minimum line is the minimum xerror plus xstd
#The first xerror that drops below the minmum line
#corresponds to the best cp - best pruned tree
plotcp(bank.ct)


#Figure 9.14
set.seed(11)
toyota.df<-read.csv("ToyotaCorolla.csv")
toyota.df$Fuel_Type<-factor(toyota.df$Fuel_Type)
toyota.selected.var<-c(3,4,7,8,9,10,12,13,14,17,18)
toyota.train.index<-sample(c(1:dim(toyota.df)[1]),dim(toyota.df)[1]*0.6)
toyota.train.df<-toyota.df[toyota.train.index,toyota.selected.var]
#Build the default (best-pruned) regression tree
toyota.default.rt<-rpart(Price~.,data=toyota.train.df,method = "anova")
#Plot tree
options(scipen = 999)
prp(toyota.default.rt,type=1,extra=1,under=TRUE,split.font = 2,
    under.font = 1,nn.font = 3,varlen = -10,
    box.col = ifelse(toyota.default.rt$frame$var=="<leaf>","gray","pink"))