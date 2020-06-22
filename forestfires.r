library(readr)
library(moments)
library(plyr)
forest<-read.csv("E:\\Assignment\\svm\\forestfires.csv")
View(forest)
colnames(forest)
sum(is.na(forest))
summary(forest)
plot(forest$FFMC,col="green")
par(mfrow=c(3,3),mar=c(2,5,2,1),las=1,bty="n")
plot(forest$FFMC,col="green")
boxplot(forest$FFMC,col = "blue")
hist(forest$DMC,col = "blue")
skewness(forest$DMC)
kurtosis(forest$DMC)
ggplot(data=train,aes(x = train$FFMC, fill = train$DMC)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train,aes(x = train$FFMC, fill = train$DMC)) +
 geom_density(alpha = 0.9, color = 'Violet')

# For SVM all the features must be in numeric 
# All the feature values should be in same range 
# If not we should normalize 
# SVM model will perform Rescalling automatically 

forest$day<-mapvalues(forest$day,from = c("mon","tue","wed","thu","fri","sat","sun"), to = c(1,2,3,4,5,6,7))
train<-forest[1:389, ]
View(train)
test<-forest[390:517, ]
test
## Model Building###
library("kernlab")
library("caret")
### model building with different kernel###
model<-ksvm(month~.,data=train,kernel="laplacedot")
pred<-predict(model,newdata=test)
mean(pred==test$month) #0.3515625
prop.table(table(pred==test$month))
##vanilladot kernel##
model1<-ksvm(month~.,data=train,kernel="vanilladot")
pred1<-predict(model1,newdata=test)
mean(pred1==test$month) ##0.71875
prop.table(table(pred1==test$month))
           
### kernel = rbfdot
model2<-ksvm(month~.,data=train,kernel="rbfdot")
pred2<-predict(model2,newdata=test)
mean(pred2==test$month) #0.59375
prop.table(table(pred2==test$month))
### kernal = besseldot
model3<-ksvm(month~.,data=train,kernel="besseldot")
pred3<-predict(model3,newdata=test)
mean(pred3==test$month) #0
prop.table(pred3==test$month)
# kernel = polydot
model4<-ksvm(month~.,data=train,kernel="polydot")
pred4<-predict(model4,newdata=test)
mean(pred4==test$month) #0.71875
prop.table(table(pred4==test$month))
## kernel= tanhdot
model5<-ksvm(month~.,data=train,kernel="anovadot")
pred5<-predict(model5,newdata=test)
mean(pred5==test$month) #0.875
prop.table(table(pred5==test$month))
plot(pred5)
