setwd("~/Desktop/Consumer Analytics/New Sessions/Session 7")
df <- read.csv("kkbox(1).csv")
df <- df[ -c(1:2) ]

skimr::skim(df)

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$is_churn, SplitRatio = 0.8)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

#Task 1 - Regression

library(MASS)
# Fit the full model 
full.model <- lm(is_churn ~., data = train)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


INPUT <-"is_churn ~ age + gender + actual_amount_paid + n_transactions + is_cancel+is_auto_renew+is_duplicate"
model = glm(formula = INPUT, family=binomial(link = "logit"), data=train)

library("margins")
x <- glm(INPUT, family=binomial(link='logit'), data=train)
(m <- margins(x))
summary(m)

model2 = glm(formula=is_churn ~ age + is_auto_renew + is_cancel + is_duplicate, family=binomial(link = "logit"), data=train)
fitted.results <-predict(model2, test, type ="response")
fitted.results <-ifelse(fitted.results > 0.1 , 1, 0)
misClasificError <- mean(fitted.results !=test$is_churn)
print(1-misClasificError)

# In my regression model, the variables "age", "is_auto_renew", "is_cancel", "is_duplicate" 
# influence the probability to churn as their p-values are <0.05 (assume 95% CI)
# This model gives a high accuracy of 90%


#Task 2 - Classification Tree
library(rpart)
train$is_churn <- factor(train$is_churn, levels = c(0,1), labels = c("No", "Yes"))
kkboxtree <- rpart::rpart(INPUT, train, method = "class")
rpart::printcp(kkboxtree)

library(rpart.plot)
rpart.plot::prp(kkboxtree, type=0, extra=100, varlen=0)

prediction <- predict(kkboxtree, test, type = "vector") - 1
misClasificError <- mean(prediction != test$is_churn)
print(1-misClasificError)
#accuracy  = 0.9285

#Pruned tree
kkboxtree2 <- rpart::rpart(INPUT, train, method = "class", control = rpart::rpart.control(minsplit = 20, cp = .015))
rpart.plot::prp(kkboxtree2, type=0, extra=4, varlen=0)

prediction2 <- predict(kkboxtree2, test, type = "vector") - 1
misClasificError <- mean(prediction2 != test$is_churn)
print(1-misClasificError)
# If the tree is "pruned", a higher accuracy can be observed, 0.931

# Which variables drive churn probability according to the trees?
# Ans: Records which aren't duplicated and they aren't on autorenew
# it gives the higher probability for that customer to churn (0.44)
# even if it is not auto renew, if the record shows that customer has cancelled the subsciption,
# it also has a high probability of churn (0.37)


library(rmarkdown)
render("Final Task.R", output_format = "word_document")




