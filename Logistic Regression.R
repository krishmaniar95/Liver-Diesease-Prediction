#Libraries:
library(car)
library(caret)
library(lattice)
library(ROCR)
library(party)
#Logistic regression MOdel:
#Creating a "0-1" column:
patients<-liver_data
colnames(patients)
#Creating the regression model:
logmod<- glm(Dataset~.,data = patients,family = binomial())
#Predicting the values:
pred<-predict(logmod, type = "response")
#Creating a confusion matrix to evaluate the results:
confusionMatrix(as.factor(patients$Dataset),as.factor(ifelse(pred>0.5,1,0)))
#Plotting the ROC Curve:
plot(performance(prediction(pred,patients$Dataset),"tpr","fpr"),main ="ROC Curve", colorize = T)

