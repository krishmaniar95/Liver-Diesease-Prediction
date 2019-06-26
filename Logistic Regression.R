#Libraries:
library(car)
library(caret)
library(lattice)
library(ROCR)
library(party)
#Logistic regression Model:
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

roc(patients$Dataset,pred, plot=TRUE, percent = TRUE, legacy.axes=TRUE, main="ROC Curve for Logistic Regression" ,xlab = "False Positive Perecentage", ylab="True Positive Rate", print.auc=TRUE)
