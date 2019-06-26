#Decision Tress:
#library
library(rpart)
library(rpart.plot)
library(maptree)
library(cluster)
#Creating the model
dtmodel<-rpart(Dataset~.,data = patients, method = "class")
printcp(dtmodel)
rpart.plot(dtmodel)
summary(dtmodel)

#Decision Tree Plot
plot(dtmodel, uniform=TRUE, main="Classification Tree for Patient Records")

#Testing the model:
dtpred<-predict(dtmodel,type = "class")

#Confusion Matrix:
confusionMatrix(dtpred,as.factor(patients$Dataset))

#ROC Plot
roc(patients$Dataset,as.numeric(dtpred), plot=TRUE, percent = TRUE, legacy.axes=TRUE, main="ROC Curve for CART ",xlab = "False Positive Perecentage", ylab="True Positive Rate", print.auc=TRUE)


