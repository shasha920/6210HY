#install packges
install.packages(c("forecast",
                   "ggplot2",
                   "gplots",
                   "reshape",
                   "GGally",
                   "MASS"))

#Library packages
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)

#Figure 3.1
#Amtrak ridership
#Line Chart
Amtrak.df<-read.csv("Amtrak.csv")
ridership.ts<-ts(Amtrak.df$Ridership,start = c(1991,1),end=c(2004,3),freq=12)
plot(ridership.ts,xlab="Year",ylab="Ridership(in 000s)",ylim=c(1300,2300))

#Boston housing
#Scatter Plot
housing.df<-read.csv("BostonHousing.csv")
plot(housing.df$MEDV ~ housing.df$LSTAT,xlab="LSTAT",ylab="MEDV")
#Alternative using ggplot
#alpha:transparency scale
ggplot(housing.df)+geom_point(aes(x=LSTAT,y=MEDV),colour="steelblue",alpha=0.5)

#Bar Chart
data.for.plot<-aggregate(housing.df$MEDV,by=list(housing.df$CHAS),FUN=mean)
names(data.for.plot)<-c("CHAS","MeanMEDV")
barplot(data.for.plot$MeanMEDV,names.arg = data.for.plot$CHAS,
        xlab="CHAS",ylab="Avg.MEDV")
#Alternative using ggplot
ggplot(data.for.plot)+geom_bar(aes(x=CHAS,y=MeanMEDV),stat = "identity")

#percentage bar chart
data.for.plot<-aggregate(housing.df$CAT..MEDV,by=list(housing.df$CHAS),FUN=mean)
names(data.for.plot)<-c("CHAS","MeanCATMEDV")
barplot(data.for.plot$MeanCATMEDV*100,names.arg = data.for.plot$CHAS,
        xlab="CHAS",ylab="% of CAT.MEDV")
#Alternative using ggplot
ggplot(data.for.plot)+geom_bar(aes(x=CHAS,y=MeanCATMEDV),stat="identity")

#Figure 3.2
#Histogram
hist(housing.df$MEDV,xlab="MEDV")
#Alternative using ggplot
ggplot(housing.df)+geom_histogram(aes(x=MEDV),binwidth = 5)

#Boxplot
boxplot(housing.df$MEDV~housing.df$CHAS,xlab="CHAS",ylab="MEDV")
#Alternative using ggplot
ggplot(housing.df)+geom_boxplot(aes(x=as.factor(CHAS),y=MEDV))+xlab("CHAS")

#Figure 3.3
#Multiple Panels
#2*2 matrix
par(mfcol=c(2,2))
boxplot(housing.df$NOX~housing.df$CAT..MEDV,xlab="CAT.MEDV",ylab="NOX")
boxplot(housing.df$LSTAT~housing.df$CAT..MEDV,xlab="CAT.MEDV",ylab = "LSTAT")
boxplot(housing.df$PTRATIO~housing.df$CAT..MEDV,xlab="CAT.MEDV",ylab="PTRATIO")
boxplot(housing.df$INDUS~housing.df$CAT..MEDV,xlab = "CAT.MEDV",ylab = "INDUS")
par(mfcol=c(1,1))

#Figure 3.4
#Heatmap
heatmap(cor(housing.df),Rowv = NA,Colv = NA)
#Alternative using gplots
#srtCol, srtRow:control angle of labels
#adjCol, adjRow:control labels justification
#cexCol,cexRow:control labels font size
#lhei,lwid:control whitespace above and to the left of matrix
#add value(-1,1)
heatmap.2(cor(housing.df),Rowv=FALSE, Colv=FALSE,dendrogram="none",
          cellnote = round(cor(housing.df),2),notecol="steelblue",
          key = FALSE,trace="none",margins = c(10,10))
#Alternative using ggplot
cor.mat<-round(cor(housing.df),2)
melted.cor.mat<-melt(cor.mat)
ggplot(melted.cor.mat,aes(x=X1,y=X2,fill=value))+
  geom_tile()+geom_text(aes(x=X1,y=X2,label=value))

#Figure 3.6
#Multidimensional Visualization
par(xpd=TRUE) #Allows drawing outside chart boarder
#pch=1 is circles
plot(housing.df$NOX~housing.df$LSTAT,ylab="NOX",xlab="LSTAT",
     col=ifelse(housing.df$CAT..MEDV==1,"black","red"))
legend("topleft",inset = c(0,-0.2),legend = c("CAT.MEDV=1","CAT.MEDV=0"),
       col=c("black","red"),pch=1,cex=0.5)
#Change the shape of symbols from circles to dots
#pch=19 is dots
plot(housing.df$NOX~housing.df$LSTAT,ylab="NOX",XLAB="LSTAT",
     col=ifelse(housing.df$CAT..MEDV==1,"black","red"),pch=19)
legend("topleft",inset = c(0,-0.2),legend=c("CAT.MEDV=1","CAT.MEDV=0"),
       col=c("black","red"),pch=19,cex=0.5)
#change the color of symbols(0="red",1="blue")
plot(housing.df$NOX~housing.df$LSTAT,ylab="NOX",xlab="LSTAT",
     col=ifelse(housing.df$CAT..MEDV==1,"blue","red"),pch=19)
legend("topleft",inset = c(0,-.02),legend=c("CAT.MEDV=1","CAT.MEDV=0"),
       col=c("blue","red"),pch=19,cex=0.5)
#Reduce the size of symbols by half
#cex is size of symbols
plot(housing.df$NOX~housing.df$LSTAT,ylab="NOX",xlab="LSTAT",
     col=ifelse(housing.df$CAT..MEDV==1,"blue","red"),pch=19,cex=0.5)
legend("topleft",inset=c(0,-0.2),legend=c("CAT.MEDV=1","CAT.MEDV=0"),
       col=c("blue","red"),pch=19,cex=0.5)
#Alternative using ggplot
ggplot(housing.df,aes(y=NOX,x=LSTAT,colour=CAT..MEDV))+geom_point(alpha=0.5)

#Multiple Panels
#Boston housing
#1*2 matrix
data.for.plot<-aggregate(housing.df$MEDV,by=list(housing.df$RAD,housing.df$CHAS),
                         FUN=mean,drop=FALSE)
names(data.for.plot)<-c("RAD","CHAS","MeanMEDV")
par(mfcol=c(1,2))
barplot(height=data.for.plot$MeanMEDV[data.for.plot$CHAS==0],
        names.arg = data.for.plot$RAD[data.for.plot$CHAS==0],
        xlab="RAD",ylab="Avg.MEDV",main="CHAS=0")
barplot(height = data.for.plot$MeanMEDV[data.for.plot$CHAS==1],
        names.arg = data.for.plot$RAD[data.for.plot$CHAS==1],
        xlab="RAD",ylab="Avg.MEDV",main="CHAS=1")
par(mfcol=c(1,1))
#Alternative using ggplot
ggplot(data.for.plot)+geom_bar(aes(x=as.factor(RAD),y=MeanMEDV),stat = "identity")+
  xlab("RAD")+facet_grid(CHAS~.)

#Figure 3.7
plot(housing.df[,c(1,3,12,13)])
#Alternative using GGally
ggpairs(housing.df[,c(1,3,12,13)])

#Figure 3.8
#Rescaling
#configure parameter "log" means rescaling
par(mfcol=c(2,2))
options(scipen=999)
plot(housing.df$MEDV~housing.df$CRIM,xlab="CRIM",ylab="MEDV")
boxplot(housing.df$CRIM~housing.df$CAT..MEDV,xlab="CAT.MEDV",ylab = "CRIM")
plot(housing.df$MEDV~housing.df$CRIM,xlab="CRIM",ylab="MEDV",log="xy")
boxplot(housing.df$CRIM~housing.df$CAT..MEDV,xlab = "CAT.MEDV",ylab = "CRIM",log="y")
par(mfcol=c(1,1))

#Figure 3.12
#Multivariate Plot
#Parallel coordinates plots
par(mfcol=c(2,1))
parcoord(housing.df[housing.df$CAT..MEDV==0,-14],main="CAT.MEDV=0")
parcoord(housing.df[housing.df$CAT..MEDV==1,-14],main="CAT.MEDV=1")
par(mfcol=c(1,1))
#Merge the parallel coordinates plots and color code the profile lines by the value of CAT..MEDV(0="red",1="black")
par(xpd=TRUE)
parcoord(housing.df[,-14],col=ifelse(housing.df$CAT..MEDV==1,"black","red"))
legend("topleft",inset=c(0,-0.1),
       legend=c("CAT.MEDV=1","CAT.MEDV=0"),
       col = c("black","red"),pch=19,cex=0.5)
