#Multiple linear regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

#Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#Fitting Multiple Linear regression to the training set
#regressor = lm(Profit ~ R.D.Spend +Administration + Marketing.Spend+State)
#or you can replace it by '.' (R understands it as independent variable)
regressor = lm(Profit ~ . ,
               data = training_set)

#Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using Backward Elimination

regressor = lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)
#Remove the 'state' variable because it is less significance 
regressor = lm(Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)
#Remove the 'Administration' variable because it is less significance 
regressor = lm(Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)
#Remove the 'Marketing.spend' variable because it is '0.06' a 
#bit higher then '0.05' i.e: more than '50%'.
regressor = lm(Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)








