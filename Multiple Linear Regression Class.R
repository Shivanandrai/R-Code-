setwd("~/Desktop/ESCP/R Class")

Fueleff <-read.csv("1-FuelEfficiency.csv")
head(Fueleff)
plot(GPM~WT, Fueleff)
model_1= lm(GPM~., data=Fueleff)
summary(model_1)

Fueleff= Fueleff[-1]
model_1= lm(GPM~., data=Fueleff)
summary(model_1)
cor(Fueleff)


library(leaps)
X=Fueleff[ ,2:7]
Y=Fueleff[ ,1]

#new function. I use Mallow's law to run the multiple regression and run the multiple variables and decide which two best models 
#it will keep
output= summary(regsubsets(X,Y, nbest=2, nvmax = ncol(X)))

tab = cbind(output$which, output$rsq, output$adjr2, output$cp)

model_2= lm(GPM~WT, data = Fueleff)
summary(model_2)

mape(Fueleff$GPM, predict(model_1,Fueleff))
mape(Fueleff$GPM, predict(model_2,Fueleff))

rmse(Fueleff$GPM, predict(model_1,Fueleff))
rmse(Fueleff$GPM, predict(model_2,Fueleff))

me(Fueleff$GPM, predict(model_1, Fueleff))
me(Fueleff$GPM, predict(model_2, Fueleff))

#split the data in a different way
ind=createDataPartition(Fueleff$GPM, p=2/3, list=FALSE)

training_data_set <- Fueleff [ind,]
testing_data_set <- Fueleff [-ind,]

model_11 = lm(GPM~., data=training_data_set)
mape(Fueleff$GPM, predict(model_11, testing_data_set))
rmse(Fueleff$GPM, predict(model_11, testing_data_set))
me(Fueleff$GPM, predict(model_11, testing_data_set))

model_22 = lm(GPM~WT, data= training_data_set)
mape(Fueleff$GPM, predict(model_11,testing_data_set))
rmse(Fueleff$GPM, predict(model_11,testing_data_set))
me(Fueleff$GPM, predict(model_11, testing_data_set))





