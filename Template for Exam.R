#

#lowers by default using Simple Least Square Regression
lines(lowess(cars$dist~cars$speed, f= 2/3))

#Loess by default fits parabolas in higher order
m2<- loess(cars$dist~cars$speed, span=0.75)
lines(cars$speed, predict(m2), col=3)

m3 <- loess(cars$dist~cars$speed, span= 0.50)
lines(cars$speed, predict(m3), col=4)

m4 <-loess(cars$dist~cars$speed, span = 0.10)
lines(cars$speed, predict(m4), col=5)

#non-local polynomial regression raw means we are not doing octoganol brute forcing
m1 <- lm(dist~poly(speed, 2, raw= TRUE), data = cars)
lines(cars$speed,predict(m1, col=6))

m5 <- lm(dist~poly(speed, 3, raw= TRUE), data = cars)
lines(cars$speed,predict(m1, col=6))


#Running a logistic regression 
linear_binom_regressor = glm(Default~., family = binomial, data = data.frame(Default=ytrain, xtrain))
summary(linear_binom_regressor)

#Predicting set

Predictive_set_linear <- predict(linear_binom_regressor, newdata = data.frame(xtest),type="response")

#Confusion Matrix at .5 threshold

accuracy<-table(ytest, gg1)

gg1=floor(Predictive_set_linear +(1/2))

error=(ttt[1,2]+ttt[2,1])/n2

#The model accuracy

model_11 = lm(GPM~., data=training_data_set)
mape(Fueleff$GPM, predict(model_11, testing_data_set))
rmse(Fueleff$GPM, predict(model_11, testing_data_set))
me(Fueleff$GPM, predict(model_11, testing_data_set))

install.packages("gplots")
library("gplots")
install.packages("ROCR")
library("ROCR")
data(ROCR.simple)
df <- data.frame(ROCR.simple)
pred <- prediction(df$predictions, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
pred2 <- prediction(abs(ROCR.simple$predictions + 
                          rnorm(length(ROCR.simple$predictions), 0, 0.1)), 
                    ROCR.simple$labels)
perf <- performance( pred, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")
plot( perf, colorize = FALSE)
plot(perf2, add = TRUE, colorize = TRUE)
