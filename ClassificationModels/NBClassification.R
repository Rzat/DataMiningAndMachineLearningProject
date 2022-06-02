rm(list=ls())

#setwd('G:/NCI Soft/DMML/PROJECT DESCRIPTOR GUIDE/Proposal Project/SupervisedLearningClassificationModel')

#Read Excel
#install.packages("readxl")
library("readxl")
dataset = read_excel('/Users/rajatthakur/Downloads/DMML/PROJECT DESCRIPTOR GUIDE/Proposal Project/SupervisedLearningClassificationModel/default of credit card clients.xls',skip=1)
head(dataset)
?read_excel()


#To check NA values
dataset[is.na(dataset)]
#To remove NA values
dataset = dataset%>% drop_na() 

#Remove Id column from dataset
#install.packages("tidyverse")
library(tidyverse)
dataset = subset(dataset, select = -c(ID))
dataset$LIMIT_BAL
sd(dataset$LIMIT_BAL)

#Rename column 'default payment next month' to 'default_payment_next_month'
colnames(dataset)[24] = 'default_payment_next_month'

summary(dataset)
View(dataset)


#install.packages("ggcorrplot")
library(ggcorrplot)
#correlation matrix
corr <- round(cor(dataset), 3)
head(corr)
ggcorrplot(corr)

#Converting dependent variable to factor
dataset$default_payment_next_month <- factor(dataset$default_payment_next_month)

class(dataset$default_payment_next_month)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$default_payment_next_month, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
View(dataset[,1:23])
training_set[,1:23] = scale(training_set[,1:23])
test_set[,1:23] = scale(test_set[,1:23])


View(training_set)
View(test_set)

# Fitting Decision Tree classification to the Training set
#https://stackoverflow.com/questions/33767804/invalid-prediction-for-rpart-object-error

#install.packages('rpart')
#install.packages('caret')
# Fitting Naive Bayesto the Training set
#install.packages("e1071")
library(e1071)
classifier = naiveBayes(x=training_set,
                        y=training_set$default_payment_next_month)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set, type="class") 

y_pred

library(caret)
# Making the Confusion Matrix
confusionMatrix(as.factor(y_pred),as.factor(test_set$default_payment_next_month),mode = 'everything')

View(test_set[-3])

install.packages('PRROC')
library(PRROC)
# plot the ROC curve (training set)
rocTest=roc.curve(test_set$default_payment_next_month, y_pred,curve = TRUE)

plot(rocTest)

library(ROSE)

roc.curve(test_set$default_payment_next_month, y_pred,plotit = TRUE)

