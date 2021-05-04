library(ISLR) # Auto data
library(e1071) # SVM

# Make binary variable for "high mpg" if larger than mean
mpg.median = median(Auto$mpg)
Auto$high_mpg = 0
Auto$high_mpg[Auto$mpg > mpg.median] = 1

# Fit support vector classifier with 10-fold CV for different costs.
costs = list(cost = c(0.01, 0.1, 1, 5, 10))

svc = svm(high_mpg~., data=Auto, cost=costs[i], kernal="linear", scale=F, k = 10)
