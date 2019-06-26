
############################################################################################
### K - Nearest Neighbours
############################################################################################

train_knn <- train_data
test_knn <- test_data
str(train_knn)
str(test_knn)

#Our target variable is 'Dataset' variable
#To remove target variable from training and testing data
train_knn2 <- train_knn[, -7]
train_knn2 <- scale(train_knn2)
test_knn2 <- test_knn[ ,-7]
test_knn2 <- scale(test_knn2)
#To check if traget variable has been removed
str(train_knn2)
str(test_knn2)

train_knn_label <- train_knn$Dataset
test_knn_label <- test_knn$Dataset
str(train_knn_label)
str(test_knn_label)

#Building a KNN model
library(class)
pred_knn <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k = 1)
table_knn <- table(test_knn_label, pred_knn)
total <- table_knn[1,1] + table_knn[2,2]
accuracy <- (total/174)*100
accuracy


#Choosing best value of K 
k1 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=1)
table1 <- table(k1, test_knn_label)
TotalCorrect1 <- table1[1,1] + table1[2,2]
Accuracy1 <- (TotalCorrect1/174)*100
print(Accuracy1)


k3 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=3)
table3 <- table(k3, test_knn_label)
TotalCorrect3 <- table3[1,1] + table3[2,2]
Accuracy3 <- (TotalCorrect3/174)*100
print(Accuracy3)

k5 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=5)
table5 <- table(k5, test_knn_label)
TotalCorrect5 <- table5[1,1] + table5[2,2]
Accuracy5 <- (TotalCorrect5/174)*100
print(Accuracy5)

k7 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=7)
table7 <- table(k7, test_knn_label)
TotalCorrect7 <- table7[1,1] + table7[2,2]
Accuracy7 <- (TotalCorrect7/174)*100
print(Accuracy7)

k9 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=9)
table9 <- table(k9, test_knn_label)
TotalCorrect9 <- table9[1,1] + table9[2,2]
Accuracy9 <- (TotalCorrect9/174)*100
print(Accuracy9)

k11 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=11)
table11 <- table(k11, test_knn_label)
TotalCorrect11 <- table11[1,1] + table11[2,2]
Accuracy11 <- (TotalCorrect11/174)*100
print(Accuracy11)

k13 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=13)
table13 <- table(k13, test_knn_label)
TotalCorrect13 <- table13[1,1] + table13[2,2]
Accuracy13 <- (TotalCorrect13/174)*100
print(Accuracy13)

k15 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=15)
table15 <- table(k15, test_knn_label)
TotalCorrect15 <- table15[1,1] + table15[2,2]
Accuracy15 <- (TotalCorrect15/174)*100
print(Accuracy15)


k17 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=17)
table17 <- table(k17, test_knn_label)
TotalCorrect17 <- table17[1,1] + table17[2,2]
Accuracy17 <- (TotalCorrect17/174)*100
print(Accuracy17)

k19 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=19)
table19 <- table(k19, test_knn_label)
TotalCorrect19 <- table19[1,1] + table19[2,2]
Accuracy19 <- (TotalCorrect19/174)*100
print(Accuracy19)

k21 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=21)
table21 <- table(k21, test_knn_label)
TotalCorrect21 <- table21[1,1] + table21[2,2]
Accuracy21 <- (TotalCorrect21/174)*100
print(Accuracy21)

k23 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=23)
table23 <- table(k23, test_knn_label)
TotalCorrect23 <- table23[1,1] + table23[2,2]
Accuracy23 <- (TotalCorrect23/174)*100
print(Accuracy23)

k25 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=25)
table25 <- table(k25, test_knn_label)
table25
TotalCorrect25 <- table25[1,1] + table25[2,2]
Accuracy25 <- (TotalCorrect25/174)*100
print(Accuracy25)

#Ploting K values and Accuracy
plot_accuracy <- c(Accuracy1, Accuracy3, Accuracy5, Accuracy7, Accuracy9, 
                   Accuracy11, Accuracy13, Accuracy15, Accuracy17, Accuracy19,
                   Accuracy21,Accuracy23,Accuracy25)
plot_Klabels <- c("K=1","K=3","K=5","K=7","K=9",
                  "K=11","K=13","K=15","K=17","K=19",
                  "K=21","K=23","K=25")
K_values <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)
plot(x=K_values,y=plot_accuracy,xlab ="K value", ylab = "Accuracy in %", 
     main="Accuracy of KNN model with varying K values")
text(x=K_values,y=plot_accuracy,labels=plot_Klabels, pos =1 )


#Metrics for best K value

error_rate <- (table25[1,2] +  table25[2,1]) / 174 
error_rate

false_pos_rate <- table25[2,1]/(table25[2,1] + table25[2,2])
false_pos_rate


#ROC Curve 
k25 <- knn(train = train_knn2, test = test_knn2, cl = train_knn_label, k=25, prob = TRUE)
k25_prob<-attr(k25, "prob")
plot(performance(prediction(k25_prob,test_knn_label),"tpr","fpr"), main = "ROC Curve for KNN", colorize = T)
roc(test_knn_label,k25_prob, percent = TRUE, main = "ROC Curve for KNN",xlab="False Positive Percentage", ylab="True Positive Percentage", print.auc=TRUE)
