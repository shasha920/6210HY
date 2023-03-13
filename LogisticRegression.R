install.packages(c("gains",
                    "caret",
                    "e1071",
                    "ggplot2",
                    "lattice"))
library(gains)
library(caret)
library(e1071)
library(ggplot2)
library(lattice)
#Table 10.2
bank.df<-read.csv("UniversalBank.csv")
#Drop ID and Zip Code which we do not need for logistic regression
bank.df<-bank.df[,-c(1,5)]
#Give Education levels appropriate names
bank.df$Education<-factor(bank.df$Education,levels=c(1,2,3),
                          labels=c("Undergrad","Graduate","Professional"))
#Partition data into training(60%) and validation (40%) sets through random sampling
set.seed(2)
train.index<-sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)
train.df<-bank.df[train.index, ]
valid.df<-bank.df[-train.index, ]
#Build logistic regression model
logit.reg<-glm(Personal.Loan ~ ., data = train.df,family = "binomial")
options(scipen=999)
summary(logit.reg)

# Table 10.3
#Prediction
#Compute propensity
pred.prob<-predict(logit.reg,valid.df[,-8], type="response")
#Classification: if equally important
pred.class<-ifelse(pred.prob>=0.5,1,0)
#Evaluating classification performance, Personal.Loan=1 is the class of interest
confusionMatrix(factor(pred.class,levels=c(1,0)),
                factor(valid.df$Personal.Loan,levels = c(1,0)))

#Figure 10.3
#Lift chart
gain<-gains(valid.df$Personal.Loan,pred.prob,groups=length(pred.prob))
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs),
       xlab="#cases",ylab="Cumulative # of responses", main="Lift Chart of Universal Bank Case",type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0,dim(valid.df)[1]),lty=2)
#Decile-wise lift chart
gain<-gains(valid.df$Personal.Loan,pred.prob)
heights<-gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints<-barplot(heights,names.arg = gain$depth,ylim=c(0,9),
                   xlab="Percentile",ylab="Mean Response",
                   main="Decile-wise lift chart")
text(midpoints,heights+0.5,labels=round(heights,1),cex=0.8)