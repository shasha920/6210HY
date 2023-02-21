#install.packages("reshape")
library(reshape)

#Table 4.3
housing.df<-read.csv("BostonHousing.csv")
head(housing.df,9)
summary(housing.df)

#Summary statistics of variable CRIM
mean(housing.df$CRIM)
sd(housing.df$CRIM)
min(housing.df$CRIM)
max(housing.df$CRIM)
median(housing.df$CRIM)
length(housing.df$CRIM)
sum(is.na(housing.df$CRIM))

#Summary statistics of everything
#organize the results in a table format
data.frame(mean=sapply(housing.df, mean),
           sd=sapply(housing.df, sd),
           min=sapply(housing.df, min),
           max=sapply(housing.df, max),
           median=sapply(housing.df, median),
           length=sapply(housing.df,length),
           miss.val=sapply(housing.df,function(x)sum(is.na(x))))

#Table 4.4
#Generate the correlation table
round(cor(housing.df),2)

#Table 4.5
#Generate frequency table of values of “CHAS”
table(housing.df$CHAS)

#Table 4.6
#Aggregate “MEDV” by “RM” and “CHAS”
housing.df$RM.bin<-.bincode(housing.df$RM,c(1:9))
aggregate(housing.df$MEDV,by=list(RM=housing.df$RM.bin,
                                  CHAS=housing.df$CHAS),FUN=mean)

#Table 4.7
#Generate a pivot table of “MEDV” by “RM” and “CHAS”
mlt<-melt(housing.df,id=c("RM.bin","CHAS"),measure="MEDV")
head(mlt)
cast(mlt,RM.bin~CHAS, subset=variable=="MEDV",
     margins=c("grand_row","grand_col"),fun.aggregate = mean)

#Table 4.8
cereals.df<-read.csv("cereals.csv")
#summary statistics of variables
summary(cereals.df)
#Compute correlation between “rating” & “calories”
cor(cereals.df[c("rating","calories")])

#Table 4.10
#PCA
#only “rating” & “calories”
pcs<-prcomp(data.frame(cereals.df$calories,cereals.df$rating))
summary(pcs)
attributes(pcs)
pcs$rotation
scores<-pcs$x
head(scores,5)

#Table 4.11
#from "protein" to "vitamins"
pcs<-prcomp(na.omit(cereals.df[,c(5:12)]))
pcs$rotation[,1:5]
pcs$x[1:5,1:5]

#Table 4.12
#Normalize (centering and scaling) 
pcs.norm<-prcomp(na.omit(cereals.df[,c(5:12)]),center = TRUE,scale. = TRUE)
summary(pcs.norm)
pcs.norm$rotation[,1:5]
pcs.norm$x[1:5,1:5]