############################################################################################
### State Vector Machine
############################################################################################

#Assigning training and testing data
train_svm <- train_data
test_svm <- test_data
str(train_svm)

#Feature Scaling
train_svm[-7] <- scale(train_data[-7])
test_svm[-7] <- scale(test_data[-7])

summary(train_svm)
summary(test_svm)

#Fitting SVM to the Training set 
library(e1071) 

svm_classifier <- svm(formula = Dataset ~ ., 
                                  data = train_svm, 
                                  type = 'C-classification', 
                                  kernel = 'linear') 
svm_classifier

#Predicting the test_svm results 
pred_svm = predict(svm_classifier, newdata = test_svm[-7]) 
pred_svm

# Making the Confusion Matrix 
table_svm = table(test_svm$Dataset, pred_svm) 
table_svm

# Plotting the training data set results 
# installing library ElemStatLearn 
library(ElemStatLearn) 
set = train_svm 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('Age', 'Albumin') 
y_grid = predict(svm_classifier, newdata = grid_set) 

plot(set[, -3], main = "SVM (Training set)", xlab = "Age", ylab = "Estimated Albumin", xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 


