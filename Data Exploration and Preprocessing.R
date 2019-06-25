library(ggplot2) 
library(readr)
library(psych)
library(corrplot)
library(ggpubr)
library(caret)
library(class)
library(gmodels)
library(plyr)

############################################################################################
###Data Exploration and Preprocessing
############################################################################################

liver_data <- read_csv("indian_liver_patient.csv")
str(liver_data)

#Change 2 to 0 in target variable
liver_data[liver_data$Dataset == 2,]$Dataset <- 0
str(liver_data)


### Data EXPLORATION
str(liver_data)
summary(liver_data)

#Age
par(mfrow=c(1,2))
hist(liver_data$Age, main = 'Age', xlab = "Age")
boxplot(liver_data$Age)
par(mfrow=c(1,1))

#Gender
par(mfrow=c(1,2))
barplot(table(liver_data$Gender), main= "Gender barplot")
par(mfrow=c(1,1))

#Total Bilirubin
par(mfrow=c(1,2))
hist(log(liver_data$Total_Bilirubin), main="Total Bilirubin ", xlab="log(total bilirubin)")
boxplot(log(liver_data$Total_Bilirubin))
par(mfrow=c(1,1))

#Direct Bilirubin
par(mfrow=c(1,2))
hist(log(liver_data$Direct_Bilirubin), main = "Direct Bilirubin ", xlab="log(Direct Bilirubin)")
boxplot(log(liver_data$Direct_Bilirubin))
par(mfrow=c(1,1))

#Alkaline Phosphotase
par(mfrow=c(1,2))
hist(log(liver_data$Alkaline_Phosphotase), main="Alkaline Phosphotase ", xlab="log(alkaline phosphotase)")
boxplot(log(liver_data$Alkaline_Phosphotase))
par(mfrow=c(1,1))

#Alamline Aminotransferase
par(mfrow=c(1,2))
hist(log(liver_data$Alamine_Aminotransferase), main="Alamine Aminotransferase ", xlab="log(Alamine Aminotransferase)")
boxplot(log(liver_data$Alamine_Aminotransferase))
par(mfrow=c(1,1))

#Asparate Amintransferase
par(mfrow=c(1,2))
hist(log(liver_data$Aspartate_Aminotransferase),main="Asparate Aminotransferase ", xlab="log(Asparate Aminotransferase")
boxplot(log(liver_data$Aspartate_Aminotransferase))
par(mfrow=c(1,1))

#Total Proteins
par(mfrow=c(1,2))
hist(liver_data$Total_Protiens,main="Total Proteins ",xlab="Total Proteins")
boxplot(liver_data$Total_Protiens)
par(mfrow=c(1,1))

#Albumin
par(mfrow=c(1,2))
hist(liver_data$Albumin, main="Albumin", xlab="Albumin")
boxplot(liver_data$Albumin)
par(mfrow=c(1,1))

#Albumin and Globulin ratio
par(mfrow=c(1,2))
hist(log(liver_data$Albumin_and_Globulin_Ratio),main="Alubmin/Globulin", xlab="log(A/G ratio)")
boxplot(log(liver_data$Albumin_and_Globulin_Ratio))
par(mfrow=c(1,1))

#Dataset(target variable)
par(mfrow=c(1,1))
barplot(table(liver_data$Dataset), main="Dataset (target variable)",
        xlab = "0: Healthy, 1:Liver problem", ylab = "Frequency")
par(mfrow=c(1,1))

#In column 'Dataset':1 <- liver patient, 0 <- healthy
table(liver_data$Dataset)


### Data Pre-processing
#Change non-numeric Gender to factor
liver_data$Gender <- factor(liver_data$Gender)
liver_data$Dataset <- factor(liver_data$Dataset)

#Check for null values
summary(liver_data)

#To remove the rows with missing data from liver_data
liver_data2 <- liver_data[complete.cases(liver_data), ]

#To verify if the null value records are removed
str(liver_data2)
summary(liver_data2)

#To check if attributes have correlation
cor_matrix <- cor(liver_data2[,-c(2,11)]) # Non-numeric fields are not considered
cor_matrix

#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients.
#In the right side of the correlogram, 
#the legend color shows the correlation coefficients and the corresponding colors
corrplot(cor_matrix,title = "Correlation Matrix",tl.col = "black",type = "upper")


#To check for linear relationship b/w two variables with high correlation
par(mfrow=c(2,2))
ggscatter(data = liver_data2, x = "Total_Bilirubin", y = "Direct_Bilirubin", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Bilirudbin", ylab = "Direct Bilirubin")

ggscatter(data = liver_data2, x = "Total_Protiens", y = "Albumin", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Protiens", ylab = "Albumin")

ggscatter(data = liver_data2, x = "Alamine_Aminotransferase", y = "Aspartate_Aminotransferase", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Alamine_Aminotransferase", ylab = "Aspartate_Aminotransferase")

ggscatter(data = liver_data2, x = "Albumin", y = "Albumin_and_Globulin_Ratio", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Albumin", ylab = "Albumin_and_Globulin_Ratio")
par(mfrow=c(1,1))


#Select the columns we need as predictors
str(liver_data2)
liver_data3 <- liver_data2[,c(1,3,6,8,9,10,11)]
colnames(liver_data3)


##Partitioning the dataset  

# 70% of the sample size
smp_size <- floor(0.70 * nrow(liver_data3))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(liver_data3)), size = smp_size)
train_data <- liver_data3[train_ind, ]
test_data <- liver_data3[-train_ind, ]

str(train_data)
str(test_data)
str(liver_data3)

