

dataset= read.csv('/Users/rajatthakur/Downloads/Car_Insurance_Claim.csv',header=T, na.strings=c(""),stringsAsFactors = T)

str(dataset)

View(dataset)

#Check NA values
dataset[is.na(dataset)]
#Remove NA values
dataset = dataset%>% drop_na() 

#Convert dependent variable to factor
dataset$OUTCOME <- factor(dataset$OUTCOME)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$OUTCOME, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

classifier = glm(formula=OUTCOME~.,
                 family=binomial,
                 data=training_set)

summary(classifier)
View(dataset)
classifier = glm(formula=OUTCOME~.- ID - RACE - EDUCATION,
                 family=binomial,
                 data=training_set)
summary(classifier)

classifier = glm(formula=OUTCOME~.- ID - RACE - EDUCATION-CREDIT_SCORE-CHILDREN,
                 family=binomial,
                 data=training_set)
summary(classifier)


classifier = glm(formula=OUTCOME~.- ID - RACE - EDUCATION-CREDIT_SCORE-CHILDREN-INCOME,
                 family=binomial,
                 data=training_set)
summary(classifier)


classifier = glm(formula=OUTCOME~.- ID - RACE - EDUCATION-CREDIT_SCORE-CHILDREN
                 -INCOME-VEHICLE_TYPE,
                 family=binomial,
                 data=training_set)
summary(classifier)


classifier = glm(formula=OUTCOME~.- ID - RACE - EDUCATION-CREDIT_SCORE-CHILDREN
                 -INCOME-VEHICLE_TYPE-DUIS,
                 family=binomial,
                 data=training_set)
summary(classifier)

classifier = glm(formula=OUTCOME~.- ID - RACE - EDUCATION-CREDIT_SCORE-CHILDREN
                 -INCOME-VEHICLE_TYPE-DUIS-SPEEDING_VIOLATIONS-AGE,
                 family=binomial,
                 data=training_set)
summary(classifier)

#Predicting the test set results
prob_pred = predict( classifier, type= 'response', newdata= test_set)

head(prob_pred)

y_pred = ifelse(prob_pred > 0.5,1,0)

#Making the confusion matrix, it will count the no. of correct and incorrect prediction, 
cm=table(test_set$OUTCOME,y_pred)
print(cm)

library(caret)
confusionMatrix(as.factor(y_pred),as.factor(test_set$OUTCOME),mode = 'everything')

library(ROSE)
roc.curve(test_set$OUTCOME, y_pred,plotit = TRUE)




