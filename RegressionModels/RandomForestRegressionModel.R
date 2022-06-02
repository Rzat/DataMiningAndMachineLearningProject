rm(list=ls())


dataset = read.csv('/Users/rajatthakur/Downloads/DMML/PROJECT DESCRIPTOR GUIDE/Proposal Project/SupervisedLearningRegressionModel/DataRFRM.csv')
head(dataset)
summary(dataset)

#Rename column names to appropriate name
colnames(dataset)[1] = 'Ambient_TemperatureAT'
colnames(dataset)[2] = 'Vacuum'
colnames(dataset)[3] = 'Atmospheric_PressureAP'
colnames(dataset)[4] = 'Relative_HumidityRH'
colnames(dataset)[5] = 'Full_Load_Electrical_Power_OutputPE'


#By using boxplot we can check do we really need to remove outliers or not.
boxplot(dataset[,c('Ambient_TemperatureAT','Atmospheric_PressureAP','Relative_HumidityRH',
                   'Vacuum','Full_Load_Electrical_Power_OutputPE')])



plot(dataset)

summary(dataset)
#sapply(dataset, class)
head(dataset)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
 library(caTools)
 set.seed(123)
 split = sample.split(dataset$Full_Load_Electrical_Power_OutputPE, SplitRatio = 0.70)
 training_set = subset(dataset, split == TRUE)
 test_set = subset(dataset, split == FALSE)


#Random Forest is a tree-based model and hence does not require feature scaling.
# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting the Random Forest Regression to the dataset
	#install.packages('randomForest')
	library(randomForest)
 ?randomForest()
	set.seed(1234)
	regressor =randomForest(formula=Full_Load_Electrical_Power_OutputPE~., data=training_set, ntree = 400)
	print(regressor)

	
#This plot shows the Error and the Number of Trees.We can easily notice that how the Error is dropping as we keep on adding more and more trees and average them.
#https://www.r-bloggers.com/2017/07/random-forests-in-r/
plot(regressor)
?which.min()
which.min(regressor$mse)
#higher the incNodePurity the more important the variable
varImpPlot(regressor)

importance(regressor)
#So according to  varImpPlot() and importance() method, we can see the AT and V are the most important variables.
regressor =randomForest(formula=Full_Load_Electrical_Power_OutputPE~Ambient_TemperatureAT+Vacuum, data=training_set, ntree = 400, proximity=TRUE)
print(regressor)

# Predicting a new result
#y_pred = predict(regressor, test_set[,1:2])
y_pred = predict(regressor, test_set)
library(caret)

#RMSE
install.packages('Metrics')
#library(Metrics)
rmse(test_set$Full_Load_Electrical_Power_OutputPE, y_pred)
#RMSE: In a regression analysis, Root Mean square error tells us about how far our anticipated values are from observed values.
#MAPE: To forecast accuracy of a model Mean Absolute Percentage Error is used.


#MAPE
#install.packages('MLmetrics')
library(MLmetrics)
MAPE(y_pred, test_set$Full_Load_Electrical_Power_OutputPE)
?MAPE()

head(y_pred)

View(y_pred)



