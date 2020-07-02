data(cars)
head(cars)
plot(cars$dist~cars$speed)

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
