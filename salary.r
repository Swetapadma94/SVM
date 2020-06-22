library(readr)
library(plyr)
library(kernlab)
library(caret)
train<-read.csv("E:\\Assignment\\svm\\salarydata_train.csv")
View(train)
test<-read.csv("E:\\Assignment\\svm\\salarydata_test.csv")
View(test)
## to merge two data frame  ##
salary<-rbind(train,test)
View(salary)
sum(is.na(salary))
summary(salary)
plot(salary)
boxplot(salary$age,col = "green")
## To make a Grid##
par(mfrow=c(3,3),mar=c(2,5,2,1),las=1,bty="n")
boxplot(salary$age,col = "green")
hist(salary$age,col = "blue")
barplot(salary$age,col = "red")
ggplot(data=train,aes(x = train$capitalloss, fill = train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=train,aes(x = train$race, fill = train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
##model Building##
model<-ksvm(train$Salary~.,data=train,kernel="vanilladot")
model
predict<-predict(model,newdata=test)
mean(predict==test$Salary) #0.8462815
table(predict==test$Salary)
prop.table(table(predict==test$Salary))
## Model Building with different kernel###
model1<-ksvm(train$Salary~.,data=train,kernel="rbfdot")
model1
predict1<-predict(model1,newdata=test)
predict1
mean(predict1==test$Salary) ## 0.8546481
table(predict1==test$Salary)
 
model2<-ksvm(train$Salary~.,data=train,kernel="polydot")
predict2<-predict(model2,newdata=test)
predict2
mean(predict2==test$Salary) #0.8462815
table(predict2==test$Salary)
 prop.table(table(predict2==test$Salary))
model3<-ksvm(train$Salary~.,data=train,kernl="tanhdot")
predict3<-predict(model3,newdata=test)
predict3
mean(predict3==test$Salary) #0.8544489
prop.table(table(predict3==test$Sal))
 plot(predict3)
 