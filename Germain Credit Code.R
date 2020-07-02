## read data and create some ‘interesting’ variables 
credit <- read.csv("X10_germancredit.csv") 
dataset <-X10_germancredit
default <- factor(dataset$Default)

levels(credit$history) = c("good","good","poor","poor","terrible") 
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"),
credit$Default <- factor(credit$Default)
## re-level the credit history and a few other variables credit$history = factor(credit$history, levels=c("A30","A31","A32",
                                                                                            labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42",
                                                  "A43","A44","A45","A46","A47","A48","A49","A410")) levels(credit$purpose) <-c("newcar","usedcar",rep("goods/repair",4),
                                                                                                                                "edu",NA,"edu","biz","biz")
## for demonstration, cut the data set to these variables
credit <- credit[,c("Default","duration","amount","installment","age",
                    "history", "purpose","foreign","rent")]
+
  xtrain))
summary(credglm)
Call:
  glm(formula = Default ~ ., family = binomial,
      data = data.frame(Default = ytrain, xtrain))
Deviance Residuals:
  Min 1Q Median 3Q Max
-2.2912 -0.7951 -0.5553 0.9922 2.2601
credit
summary(credit) # check out the data
## create a design matrix
## factor variables are turned into indicator variables ## the first column of ones is omitted
Xcred <- model.matrix(Default~.,data=credit)[,-1] Xcred[1:3,]