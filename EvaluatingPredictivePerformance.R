install.packages(c("forecast","gains","caret"))
library(forecast)
library(gains)
library(caret)
library(ggplot2)
library(lattice)

#Table 5.1
#Prediction Performance
toyota.df<-read.csv("ToyotaCorolla.csv")
#everytime when training and validation set seed in R
set.seed(1)
#choose sample
training<-sample(toyota.df$Id,600)
validation<-sample(setdiff(toyota.df$Id,training),400)
#made linear regression
reg<-lm(Price~.,data=toyota.df[,-c(1,2,8,11)],
        subset=training,na.action = na.exclude)
#made predictor
pred_t<-predict(reg,na.action = na.pass)
pred_v<-predict(reg,newdata=toyota.df[validation,-c(1,2,8,11)],
                na.action = na.pass)
#made accuracy
accuracy(pred_t,toyota.df[training,]$Price)
accuracy(pred_v,toyota.df[validation,]$Price)
#Draw histogram and boxplots of errors of both training and validation sets
err_t<-toyota.df[training,]$Price-pred_t
err_v<-toyota.df[validation,]$Price-pred_v
par(mfcol=c(1,3))
hist(err_t,breaks=50,main="Histogram of Error",xlab="Error(Training)")
hist(err_v,breaks = 50,main = "Histogram of Error",
     xlab="Error (Validation)",xlim = c(-3000,3000))
boxplot(list(Train.=err_t,valid.=err_v),main="Boxplots of Error")
par(mfcol=c(1,1))

#Figure 5.2
#Ranking Performance
set.seed(1)
training<-sample(toyota.df$Id,600)
validation<-sample(setdiff(toyota.df$Id,training),400)
reg<-lm(Price~.,data=toyota.df[,-c(1,2,8,11)],
        subset = training)
pred_v<-predict(reg,newdata = toyota.df[validation,-c(1,2,8,11)])
gain<-gains(toyota.df[validation,]$Price[!is.na(pred_v)],pred_v[!is.na(pred_v)])
options(scipen = 999)
price<-toyota.df[validation,]$Price[!is.na(toyota.df[validation,]$Price)]
#Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab="# cases",ylab="Cumulative Price",main="Lift Chart",type="l")
lines(c(0,sum(price))~c(0,dim(toyota.df[validation,])[1]),col="steelblue",lty=4)
#Decile-wise lift chart
barplot(gain$mean.resp/mean(price),names.arg = gain$depth,
        xlab = "Percentile",ylab="Mean Resonse",main="Decile-wise lift chart")

#Figure 5.6
