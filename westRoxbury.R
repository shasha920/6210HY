#Loading and looking at the data Table 2.3
housing.df<-read.csv("WestRoxbury.csv",header=TRUE)
dim(housing.df)
head(housing.df)
View(housing.df)
housing.df[1:10,1]
housing.df[1:10, ]
housing.df[5,1:10]
housing.df[5,c(1:2,4,8:10)]
housing.df[,1]
housing.df$TOTAL.VALUE
housing.df$TOTAL.VALUE[1:10]
length(housing.df$TOTAL.VALUE)
mean(housing.df$TOTAL.VALUE)
summary(housing.df)

#Sampling Table 2.4
#When choose sampling, set seed
set.seed(100)
s<-sample(row.names(housing.df),5)
housing.df[s,]
#Oversampling
s<-sample(row.names(housing.df),5,prob=ifelse(housing.df$ROOMS>10,0.9,0.01))
housing.df[s,]

#Handling categorical variables Table 2.5
names(housing.df)
t(t(names(housing.df)))
class(housing.df$REMODEL)#not a facotr yet
housing.df$REMODEL<-factor(housing.df$REMODEL)
class(housing.df$REMODEL)
class(housing.df[,14])
levels(housing.df[,14])
class(housing.df$BEDROOMS)
class(housing.df[,1])

#Table 2.6
xtotal<-model.matrix(~0+BEDROOMS+REMODEL,data=housing.df)
xtotal$BEDROOMS[1:5]#will not work since xtotal is a matrix but not data frame
xtotal<-as.data.frame(xtotal)
t(t(names(xtotal)))
head(xtotal)
xtotal<-xtotal[,-4]
head(xtotal)

#Missing values Table 2.7
#test missing values is.na()
rows.to.missing<-sample(row.names(housing.df),10)
housing.df[rows.to.missing,]$BEDROOMS<-NA
summary(housing.df$BEDROOMS)
#handing missing values to median
housing.df[rows.to.missing,]$BEDROOMS<-median(housing.df$BEDROOMS,na.rm=TRUE)
summary(housing.df$BEDROOMS)

#Opartition the data Table 2.9
#when train, set seed
set.seed(1)
#Traing 60%
train.rows<-sample(rownames(housing.df),dim(housing.df)[1]*0.6)
train.data<-housing.df[train.rows,]
#validation 40%
valid.rows<-setdiff(rownames(housing.df),train.rows)
valid.data<-housing.df[valid.rows,]

#Training 50%
train.rows<-sample(rownames(housing.df),dim(housing.df)[1]*0.5)
#partition validation 30%
valid.rows<-sample(setdiff(rownames(housing.df),train.rows),dim(housing.df)[1]*0.3)
#the rest partition test 20%
test.rows<-setdiff(rownames(housing.df),union(train.rows,valid.rows))
train.data<-housing.df[train.rows,]
valid.data<-housing.df[valid.rows,]
test.data<-housing.df[test.rows,]

#Multiple linear regression Table 2.11
#Reload dataset
housing.df<-read.csv("WestRoxbury.csv",header = TRUE)
#Remove "TAX" variable since it is inappropriate
#to be used as independent variable
housing.df<-housing.df[,-which(names(housing.df)=="TAX")]
#Training 70%
set.seed(1)
train.rows<-sample(rownames(housing.df),dim(housing.df)[1]*0.7)
#validation 25%
valid.rows<-sample(setdiff(rownames(housing.df),train.rows),dim(housing.df)[1]*0.25)
#test 5%
test.rows<-setdiff(rownames(housing.df),union(train.rows,valid.rows))
train.data<-housing.df[train.rows,]
valid.data<-housing.df[valid.rows,]
test.data<-housing.df[test.rows,]
#Build a multiple Linear regression model
model<-lm(TOTAL.VALUE~.,data=housing.df,subset = train.rows)
summary(model)
#Apply the model to predict total value in training data
train.results<-data.frame(train.data$TOTAL.VALUE,model$fitted.values,model$residuals)
head(train.results)
#Apply the model to predict total value in validation data
pred<-predict(model,newdata = valid.data)
valid.results<-data.frame(valid.data$TOTAL.VALUE,model.pred.values=pred,
                          model.residuals=valid.data$TOTAL.VALUE-pred)
head(valid.results)

#Interpret the results of the algorithms
#Evaluate training and validation performance Table 2.13
#install.packages("forecast")
library(forecast)
#Training performance
accuracy(model$fitted.values,train.data$TOTAL.VALUE)
#Apply the model to predict total value in new (test) data
pred<-predict(model,newdata = test.data)
test.results<-data.frame(model.pred.values=pred,test.data[,-1])
head(test.results)