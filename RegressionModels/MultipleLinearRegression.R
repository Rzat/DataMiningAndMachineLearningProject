
# Importing the dataset
dataset = read.csv('/Users/rajatthakur/Downloads/DMML/PROJECT DESCRIPTOR GUIDE/Proposal Project/SupervisedLearningRegressionModel/DataRFRM.csv')


#Rename column names to appropriate name
colnames(dataset)[1] = 'Ambient_TemperatureAT'
colnames(dataset)[2] = 'Vacuum'
colnames(dataset)[3] = 'Atmospheric_PressureAP'
colnames(dataset)[4] = 'Relative_HumidityRH'
colnames(dataset)[5] = 'Full_Load_Electrical_Power_OutputPE'

head(dataset)

#To check NA values
dataset[is.na(dataset)]

str(dataset)

#returns the first-appearing value of the set of modes
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(dataset$Ambient_TemperatureAT)


#By using boxplot we can check about outliers whether ther are present in our dataset or not.
boxplot(dataset[,c('Ambient_TemperatureAT','Atmospheric_PressureAP','Relative_HumidityRH',
                   'Vacuum','Full_Load_Electrical_Power_OutputPE')])



plot(dataset)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Full_Load_Electrical_Power_OutputPE, SplitRatio = 0.70)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)



#Fitting Multiple LInear Regression to the Training set
regressor = lm(formula= Full_Load_Electrical_Power_OutputPE~.,
               data=training_set)

summary(regressor)

#Predicting the test results
y_pred= predict(regressor, newdata= test_set )

View(y_pred)

#RMSE
install.packages('Metrics')
library(Metrics)
rmse(test_set$Full_Load_Electrical_Power_OutputPE, y_pred)

#MAPE
install.packages('MLmetrics')
library(MLmetrics)
MAPE(y_pred, test_set$Full_Load_Electrical_Power_OutputPE)
?MAPE()

#RSS
summary(regressor)$r.squared


head(y_pred)

View(test_set)

#
par(mfrow=c(2,2))
plot(regressor)

library(car)
ncvTest(regressor)
?ncvTest()

vif(regressor)

durbinWatsonTest(regressor)

cooks.distance(regressor)

