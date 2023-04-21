pwd()
install.packages(bsda)
library(ggplot2)
library(BSDA)
library("readxl")
library("ggpubr")
library(BSDA)
library(magrittr)
library(tidyverse)
library(Metrics)
library(caret)
library(e1071)
library(randomForest)

data <- read.csv("C:/Users/Desktop/laptop_data.csv")

print(data)
data = data.frame(data)
df <- data[,-1]
View(df)
summary(df)
dim(df)
#Data Visualization
df$Ram <- gsub("GB","",df$Ram)
df$Ram <- as.numeric(as.character(df$Ram))
df$Weight <- gsub("kg","",df$Weight)
df$Weight <- as.numeric(as.character(df$Weight))
den <- density(df$Price)
plot(den, frame = FALSE, col ="blue",main = "price density plot")
ggplot(df , aes(x=Price))+ geom_density() + scale_x_log10(breaks = c(10000,40000,80000,120000))
hist(df$Price,x_val = "price", col="red")
hist(df$Price,x_val="price",col="green",prob='TRUE',xlab='price')
lines(density(df$Price),lwd=2,col='black')

d <- tapply(df$Price, df$Company ,mean)
barplot(d)

d <- tapply(df$Price, df$TypeName, mean)
barplot(d)

plot(df$Inches,df$Price,col='green')
mdata <- read_excel("C:/Users/Desktop/laptop_data.xlsx")
View(mdata)
d <- tapply(mdata$Price,mdata$V1.y, mean)
barplot(d)

table(mdata$V1.x)
d <- tapply(mdata$Price,mdata$V1.x, mean)
barplot(d)

d <- tapply(df$Price, df$Ram,mean)
barplot(d)

df<-sample(mdata)
df
View(df)
#converting
df$mem_HDD <- as.numeric(as.character(df$mem_HDD))
df$mem_SSD <- as.numeric(as.character(df$mem_SSD))
df$mem_flash <- as.numeric(as.character(df$mem_flash))
df$Ram <- as.numeric(as.character(df$Ram))
df$x_res <- as.numeric(as.character(df$x_res))
df$y_res <- as.numeric(as.character(df$y_res))
df$Inches <- as.numeric(as.character(df$Inches))
df$Weight <- as.numeric(as.character(df$Weight))
df$Ram <- gsub("GB","",df$Ram)
df$Ram <- as.numeric(as.character(df$Ram))
df$Weight <- gsub("kg","",df$Weight)
df$Weight <- as.numeric(as.character(df$Weight))

#Testing

#shapiro test for normality, if p>0.05 accept H0 else reject
shapiro.test(df$Ram)

#chi square test - test for independence between categorical variables
#null hypothesis-variables do not have significant relation
#If p>0.05 accept H0 else reject
chisq.test(df$Company,df$Price, correct = FALSE,simulate.p.value = TRUE)
chisq.test(df$OpSys,df$Price, correct = FALSE,simulate.p.value = TRUE)
chisq.test(df$TypeName,df$Price, correct = FALSE,simulate.p.value = TRUE)
chisq.test(df$V1.x,df$Price, correct = FALSE,simulate.p.value = TRUE)
chisq.test(x = df$Touch,y = df$Price, simulate.p.value = TRUE)

cor(df$Inches,df$Price)
cor(df$Ram,df$Price)
cor(df$mem_SSD,df$Price)
cor(df$mem_flash,df$Price)

#for ram
summary(df$Ram)
#Pearson - linear correlation between two variables X and Y
cor.test(df$Price, df$Ram, method=c("pearson"))
#T-test - used to determine whether the mean of the two groups sampled from normal distribution is equal to each other. 
#null hypothesis is that the two means are the same, if p>0.05 accept H0 else reject
t.test(x=df$Ram,conf.level=0.05)

#data pre-process
library(stringr)
df$V2.x <- as.numeric(str_remove(df$V2.x,'GHz'))

#split test and train
set.seed(120)
View(df)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
View(train)
View(test)

#model build
model <- lm(unlist(Price) ~ unlist(Company)+unlist(TypeName)+unlist(Inches)+unlist(Ram)+unlist(OpSys)+unlist(Weight)+unlist(V1.x)+unlist(V2.x)+unlist(V1.y)+unlist(mem_HDD)+unlist(mem_SSD)+unlist(mem_flash)+unlist(Touch)+unlist(x_res)+unlist(y_res), data = train )
summary(model)
summary(model)$coef
plot(model)

model2<- lm(unlist(Price) ~ unlist(Inches)+unlist(Ram)+unlist(Weight)+unlist(V1.x)+unlist(mem_SSD)+unlist(Touch), data = train)
summary(model2)
summary(model2)$coef
plot(model2)
plot(x=df$Ram, y=df$Price, type='lm')
test1 <- select(test,Inches,Ram, Weight,V1.x,mem_SSD,Touch )

pred = predict(model2, test1)
print(pred)
mn = mean(df$Price,na.rm = TRUE)
rm = rmse(test$Price,pred)
print(rm/mn)

model3<-svm(unlist(log(Price)) ~ unlist(Inches)+unlist(Ram)+unlist(Weight)+unlist(V1.x)+unlist(mem_SSD)+unlist(Touch), data = train)
predsvm=predict(model3,test1)
mn = mean(df$Price,na.rm = TRUE)
rm = rmse(test$Price,predsvm)
print(rm/mn)

model4<-randomForest(Price~.,data=train,ntree=100)
model4
summary(model4)
predrf=predict(model4,test)
mn = mean(df$Price,na.rm = TRUE)
rm = rmse(test$Price,predrf)
print(rm/mn)
plot(model4)
importance(model4)
varImpPlot(model4)