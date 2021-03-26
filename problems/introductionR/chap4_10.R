library(ISLR)
library(gam)
data = Weekly

fix(data)
plot(data)
names(data)

fit.logreg = glm(Direction~. -Today -Year, data=data, family = binomial)
summary(fit.logreg)

# Seems to be a poor fit, Lag2 is the only one seeming to have some correlation.
fit.logreg.pred = fit.logreg$fitted.values
fit.logreg.probs = predict(fit.logreg, type="response")

fit.logreg.pred = rep("Down", length(fit.logreg.pred))
fit.logreg.pred[fit.logreg.probs > 0.5] = "Up"

confusion = table(fit.logreg.pred, data$Direction)
correct.down = confusion[1,1]/(confusion[1,1]+confusion[2,1])
correct.up = confusion[2,2]/(confusion[2,2]+confusion[1,2])
# It seems that logistic regression is pretty good at predicting up, but very bad at predicting down.
# The amount of "true positive" ups is probably due to it guessing A LOT of up. 

# Test/Train split at first occurrence of 2009
split = which(data$Year == 2009)[1]
data.train = data[1:split-1,]
data.test = data[split:nrow(data),]

glm.fit = glm(Direction~Lag2, data=data, family=binomial)

glm.prob = predict(glm.fit, newdata=data.test)
glm.pred = rep("Down", length(glm.prob))
glm.pred[glm.prob > 0.5] = "Up"

table(glm.pred, data.test$Direction)
# Still pretty bad

#LDA
library(MASS)
train = (data$Year < 2009)
tail(data[train,])
