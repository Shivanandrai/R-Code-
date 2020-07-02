# Importing the dataset
library(readr)
dataset <- read_csv("data for problem A.csv")

# Encoding the Sex feature as factor
dataset$`Sex / nominal, M, F, and I (infant)` = factor(dataset$`Sex / nominal, M, F, and I (infant)`, levels = c('M', 'F', 'I'))

# Encoding the real age into the rings column
dataset$`Rings / integer / +1.5 gives the age in years`=dataset$`Rings / integer / +1.5 gives the age in years`+1.5

#rename the dependent variable columns
library(plyr)
dataset= rename(dataset, c(`Rings / integer / +1.5 gives the age in years`="Age"))
                 
#feature scaling the continuous variables
dataset[-1] = scale(dataset[-1])

# Splitting the dataset into the Training set and Test set
#Packages and Libraries to install [remove # in the two lines below to do so]
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Age, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Splitting the training set into the independent variable and the dependent variables
y_train = training_set[9]
x_train= training_set[-9]

#Splitting the test set into the independent variable and the dependent variables
y_test = test_set[9]
test_set = test_set[-9]

#Running a SVM Model 
#Packages and Libraries to install [remove # in the two lines below to do so]
#install.packages('e1071')
library(e1071)
model_svm= svm(formula = Age ~ .,
               data = training_set,
               type = 'eps-regression',
               kernel = 'linear')

y_pred_svm= predict(model_svm, test_set)

summary(y_pred_svm)

#Running a Linear regression Model

linear_regressor = glm(Age~., data= training_set)
y_pred_linear= predict(linear_regressor, test_set)

#summary of regressors
summary(linear_regressor)
summary(model_svm)

#Scores for Regressors

y_pred1= data.frame(y_pred_linear)
y_pred2= data.frame(y_pred_svm)
y_test= data.frame(y_test)


#install.packages('Metrics')
library(Metrics)
rmse(y_test[[1]], y_pred_linear[[1]])
rmse(y_test,y_pred)


rmse1 <- function(actual, pred){
  return(sqrt(mean((pred[[1]]-actual[[1]])^2)))
}
rmse1(y_test[[1]], y_pred_linear[[1]])