rmse <- function(actual, pred){
  return(sqrt(mean((pred[[1]]-actual[[1]])^2)))
}

library(DMwR)

smote<-as.data.frame(Lending_club_Train)
smote$member_id <- NULL
smote[, c()]<- factor(smote$emp_title)
smote <- SMOTE(loan_status~., data=smote, perc.over = 100, perc.under=200)
