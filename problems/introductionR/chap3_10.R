library(ISLR)

fit1 = lm(Sales~Price+Urban+US, data=Carseats)
summary(fit1)

# Price coef is negative. If the price increases, the sales decrease. People don't want to buy many expensive things
# UrbanYes is negativ. Presumably the urban population buys less things, but since the t value is very low, this is prob not stat-sig
# USTes has a relativly large t state and low p value. If the person is from the US, they will be more likely to buy something?

# We reject the NULL HYPO for Price and US. The P value for Urban is 0.936 and is very that the NULL HYPO is true here

fit2 = lm(Sales~Price+US, data=Carseats)
summary(fit2)

MSE = function(fit) {
  mean(fit$residuals^2)
}
MSE_1 = MSE(fit1)
MSE_2 = MSE(fit2)

confint(fit1, level=0.95)
confint(fit2, level=0.95)
library(ggplot2)
ggplot(data=Carseats) + 
  geom_point(aes(x=Price, y=Sales, color=US, shape=Urban))

