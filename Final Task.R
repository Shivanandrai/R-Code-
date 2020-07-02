df <- read.csv("data_svedka(1).csv", sep = ";")

skimr::skim(df)

Question_1 <- lm(LnSales ~ LnLSales, df)
summary(Question_1)

#The current sales are highly dependent on the previous term's sales

Question_2 <- lm(LnDiff ~ LnLPrice + Tier1 + Tier2, df)

summary(Question_2)

#Quality has a positive impact of sales

Question_3 <- lm(LnSales ~  LnPrint + LnOut + LnBroad + LagTotalSales + Tier1 + Tier2, df)

summary(Question_3)

Question_4 <- lm(LnSales ~  LagTotalMinusSales+ LnPrint + LnOut + LnBroad + LagTotalSales + Tier1 + Tier2, df)
summary(Question_4)

Question_5 <- lm(LnSales ~  Firstintro + LagTotalMinusSales+ LnPrint + LnOut + LnBroad + LagTotalSales + Tier1 + Tier2, df)
summary(Question_5)

library(rmarkdown)
render("Final Task.R", output_format = "word_document")