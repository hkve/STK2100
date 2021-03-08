library(ISLR)
library(MASS)
library(leaps)
train_size = 0.8
train = sample(seq(1:nrow(College)), size = nrow(College)*train_size, replace=FALSE)

data.train = College[train, ]
data.test = College[-train, ]


fit.forward = regsubsets(Outstate~., data=data.train, nbest=1,method="forward", nvmax=17)
reg.summary = summary(fit.forward)

names(reg.summary)

par(mfrow=c(1,3))
# Cp
cp.idx = which.min(reg.summary$cp)
plot(reg.summary$cp, xlab="Number of variables", ylab="Cp", type="l")
points(cp.idx, reg.summary$cp[cp.idx], col="red", pch=19)

# AdjR2
adjr2.idx = which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab="Number of variables", ylab="Adjusted R^2", type="l")
points(adjr2.idx, reg.summary$adjr2[adjr2.idx], col="red", pch=19)

# Bic
bic.idx = which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of variables", ylab="BIC", type="l")
points(bic.idx, reg.summary$bic[bic.idx], col="red", pch=19)

sprintf("Number of variables from: Cp: %i, adjr2: %d, bic: %i", cp.idx, adjr2.idx, bic.idx)
coefs = coef(fit.forward, id= 8)
coefs

names(College)
fix(College)

# GAM with smoothing splines 
library(gam)
fit.gam1 = gam(Outstate~Private+s(Room.Board, df=2)+s(PhD, df=2)+s(perc.alumni, df=2)+s(Expend, df=2)+s(Grad.Rate, df=2)+s(Terminal, df=2)+s(Personal, df=2), data=data.train)
par(mfrow=c(2,4))
plot(fit.gam1, se = T, col="blue")

# perc.alumnim, Room.Board, Grad.Rate seems to most linear.
Outstate.predict = predict(fit.gam1, data.test)

TSS = sum((Outstate.predict-mean(data.test$Outstate))^2)
RSS = sum((Outstate.predict-data.test$Outstate)^2)
R2 = 1 - RSS/TSS

summary(fit.gam1)
