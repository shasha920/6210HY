install.packages('ggplot2')
library(ggplot2)
install.packages('e1071')
library(e1071)
install.packages('lattice')
library(lattice)
install.packages('caret')
library(caret)
install.packages('gains')
library(gains)

#Table 8.4
delays.df<-read.csv("FlightDelays.csv")
#Declare the following variables as factors
#Starting from R 4.0.0 character variables are not reated
#as factors anymore(default.stringAsFactors()=False)
delays.df$CARRIER<-factor(delays.df$CARRIER)
delays.df$DEST<-factor(delays.df$DEST)
delays.df$ORIGIN<-factor(delays.df$ORIGIN)
delays.df$DAY_WEEK<-factor(delays.df$DAY_WEEK)
delays.df$Flight.Status<-factor(delays.df$Flight.Status)
#Convert departure time from military time format to integer 24-hours format
#and declare departure time as factor, which was integer
#Like 14:55 become 15
delays.df$CRS_DEP_TIME<-factor(round(delays.df$CRS_DEP_TIME/100))
#select a subset of variables for classification
selected.var<-c(10,1,8,4,2,13)
names(delays.df)[selected.var]
#Create training (60%) and validation (40%) sets by random sampling
set.seed(1)
train.index<-sample(c(1:dim(delays.df)[1]),dim(delays.df)[1]*0.6)
train.df<-delays.df[train.index,selected.var]
valid.df<-delays.df[-train.index,selected.var]
#Run naive bayes classifier
delays.nb<-naiveBayes(Flight.Status~.,data=train.df)
delays.nb

#Table 8.5
#Display pivot table: probability of destination conditioned on flight status
#margin=1:normalize elements by row (all elements in the same row sum to 1)
prop.table(table(train.df$Flight.Status,train.df$DEST),margin=1)
#or equivalently, the same table can be retrieved from delays.nb object
delays.nb$tables$DEST

#Table 8.6
#Scoring (compute the propensity) new records
pred.prob<-predict(delays.nb,newdata=valid.df,type="raw")
#Classify new records
pred.class<-predict(delays.nb,newdata=valid.df,type="class")
df<-data.frame(actual=valid.df$Flight.Status,predicted=pred.class,prob=pred.prob)
df[valid.df$CARRIER=="DL"
   &valid.df$DAY_WEEK==7
   &valid.df$CRS_DEP_TIME==10
   &valid.df$DEST=="LGA"
   &valid.df$ORIGIN=="DCA",]

#Table 8.7
#Performance evaluation of training set
pred.class<-predict(delays.nb,newdata=train.df)
confusionMatrix(pred.class,train.df$Flight.Status)
#Performance evaluation of validation set
pred.class<-predict(delays.nb,newdata = valid.df)
confusionMatrix(pred.class,valid.df$Flight.Status)

#Figure 8.1
#Draw lift chart to visualize ranking performance using validation set
gain<-gains(ifelse(valid.df$Flight.Status=="delayed",1,0),pred.prob[,1],groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed"))~c(0,gain$cume.obs),
     xlab="# of cases", ylab="Cumulative # of delays detected",mian="",type='l')
lines(c(0,sum(valid.df$Flight.Status=="delayed"))~c(0,dim(valid.df)[1]),lty=4)