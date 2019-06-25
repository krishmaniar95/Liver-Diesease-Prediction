#Decision Tress:
#library
library(rpart)
library(rpart.plot)
library(maptree)
library(cluster)
#Creating the model
dtmodel<-rpart(Diagnosis~.,data = patients, method = "class")
printcp(dtmodel)
rpart.plot(dtmodel)
summary(dtmodel)

#Decision Tree Plot
plot(dtmodel, uniform=TRUE, main="Classification Tree for Patient Records")

#Testing the model:
dtpred<-predict(dtmodel,type = "class")

#Confusion Matrix:
confusionMatrix(dtpred,as.factor(patients$Diagnosis))

#ROC Plot
plot(performance(prediction(as.numeric(dtpred),patients$Diagnosis),"tpr","fpr"),main ="ROC Curve", colorize = T)

