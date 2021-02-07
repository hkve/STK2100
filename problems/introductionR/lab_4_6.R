library(ISLR)
attach(Smarket)

cor(Smarket[, -9])
Smarket$Direction = as.factor(Smarket$Direction)

plot(Year, Volume)

fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data=Smarket)
summary(fit)

fit.probs = predict(fit, type="response")
print(fit.probs[1:10])
contrasts(Direction)
