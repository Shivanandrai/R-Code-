#call the data
credit <- read.csv("data for practice.csv") 
credit$Default <- factor(credit$Default)

#relabel the datset
credit$history = factor(credit$history, levels =c("A30", "A31", "A32", "A33", "A34"))
levels(credit$history)= c("good","good", "poor", "poor", "terrible")
credit$foreign <- factor(credit$foreign, levels = c("A201", "A202"), labels = c("foreign","german"))
credit$rent <- factor (credit$housing=="A151")
credit$purpose = factor(credit$purpose, levels =c("A40", "A41", "A42", "A43", "A44","A45", "A46", "A47", "A48", "A49", "A410"))
levels(credit$purpose) <- c("newcar","usedcar", rep("goods/repair",4), "edu", NA,"edu","biz","biz")
credit$job = factor(credit$job, levels =c("A171", "A172", "A173", "A174", "A175"))
levels(credit$job)= c("unskilled_nonresident","unskilled_resident", "skilled", "management_self-employed", "officer")

#final dataset
credit <- credit [,c("Default", "duration","amount","installment", "age", "history", "purpose", "foreign", "rent", "job")]
summary(credit)

Xcred <- model.matrix(Default~., data = credit)[,-1]
Xcred[1:3,]

#training & Test set
set.seed (123)
train <-sample(1:1000, 900)
xtrain <- Xcred[train,]
xtest <- Xcred[-train,]
ytrain <-credit$Default[train]
ytest <- credit$Default [-train]

linear_binom_regressor = glm(Default~., family = binomial, data = data.frame(Default=ytrain, xtrain))
summary(linear_binom_regressor)

#testing set

Predictive_set_linear <- predict(linear_binom_regressor, newdata = data.frame(xtest),type="response")


accuracy<-table(ytest, gg1)

gg1=floor(Predictive_set_linear +(1/2))

accuracy