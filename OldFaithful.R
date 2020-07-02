install.packages("locfit")
library(locfit)

## local polynomial regression of TimeEruption on TimeWaiting
plot(TimeWaiting~TimeEruption,data=X3_OldFaithful)
# standard regression fit
fitreg=lm(TimeWaiting~TimeEruption,data=X3_OldFaithful)
plot(TimeWaiting~TimeEruption,data=X3_OldFaithful)
abline(fitreg)
# fit with nearest neighbor bandwidth
fit3 <- locfit(TimeWaiting~lp(TimeEruption),data=X3_OldFaithful)
plot(fit3)

plot(TimeWaiting~TimeEruption,data=X3_OldFaithful)
lines(fit3, col=2)
abline(fitreg)

##density histograms & Time of Eruption
hist(X3_OldFaithful$TimeEruption, freq = FALSE)
fit1 <-locfit(~lp(TimeEruption), data= X3_OldFaithful)
lines(fit1, col=2)