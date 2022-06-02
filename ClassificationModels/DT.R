
#Read Excel
#install.packages("readxl")
library("readxl")
dataset = read_excel('/Users/rajatthakur/Downloads/DMML/PROJECT DESCRIPTOR GUIDE/Proposal Project/SupervisedLearningClassificationModel/default of credit card clients.xls',skip=1)
head(dataset)
?read_excel()

#Remove Id column from dataset
#install.packages("tidyverse")
library(tidyverse)
dataset = subset(dataset, select = -c(ID))


#Rename column 'default payment next month' to 'default_payment_next_month'
colnames(dataset)[24] = 'default_payment_next_month'

summary(dataset)
View(dataset)

#To check NA values
dataset[is.na(dataset)]

#To remove NA values
dataset = dataset%>% drop_na() 

#install.packages("ggcorrplot")
library(ggcorrplot)
#correlation matrix
corr <- round(cor(dataset), 3)
head(corr)
ggcorrplot(corr)


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
training_set[,1:23] = scale(training_set[,1:23])
test_set[,1:23] = scale(test_set[,1:23])


View(training_set)
View(test_set)

# Fitting Decision Tree classification to the Training set
#https://stackoverflow.com/questions/33767804/invalid-prediction-for-rpart-object-error

#install.packages('rpart')
#install.packages('caret')
library(caret)
library(rpart)
classifier = rpart(formula= default_payment_next_month~BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+
                   BILL_AMT5+BILL_AMT6+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6,
				data = training_set)


classifier = rpart(formula= default_payment_next_month~.,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set, type="class") 

y_pred

library(caret)
# Making the Confusion Matrix
confusionMatrix(as.factor(y_pred),as.factor(test_set$default_payment_next_month),mode = 'everything')

install.packages('PRROC')
library(PRROC)

plot(rocTest)

library(ROSE)

roc.curve(test_set$default_payment_next_month, y_pred,plotit = TRUE)



# Visualising the Training set results
install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type='class')
plot(set[, -3],
     main = 'Decision Tree (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type='class')
plot(set[, -3], main = 'Decision Tree (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))




#Plotting the Decision Tree, before executing below lines of code please disable Feature scaling
plot(classifier)
text(classifier)
