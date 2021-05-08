library(ISLR) # Auto data
library(e1071) # SVM

# Make binary variable for "high mpg" if larger than mean
mpg.median = median(Auto$mpg)
Auto$high_mpg = 0
Auto$high_mpg[Auto$mpg > mpg.median] = 1
Auto$high_mpg = as.factor(Auto$high_mpg)
# Fit support vector classifier with 10-fold CV for different costs.
costs = list(cost = c(0.01, 0.1, 1, 5, 10, 20, 50, 70, 100))

# Using tune we can try different cost's (C) and use 10-fold CV
set.seed(1)
tc= tune.control(cross=10)
tune.out = tune(svm, high_mpg~., data=Auto, kernal="linear", ranges=costs, tunecontrol = tc)
summary(tune.out)

# C = 1 and C = 70 seems to preform the best for the SVC, with a CV mse=0.01025641.

# We will now fit a SVM with polynomial kernal
cost_and_deg = list(cost = c(0.01, 0.1, 1, 5, 10, 20, 50, 70, 100), degree = c(2,3,4,5))
tune.out.poly = tune(svm, high_mpg~., kernal="polynomial", data = Auto, ranges=cost_and_deg)
summary(tune.out.poly)

# We find best preformance for C = 100 and degree = 2, with a CV mse=0.01019231. 
# This is just sligthly better than the linear kernal and no more real preformance increase was achived.

# Finally we try a radial kernal
cost_and_gamma = list(cost = c(0.01, 0.1, 1, 5, 10, 20, 50, 70, 100), gamma=c(0.001, 0.005, 0.01, 0.05, 0.1))
tune.out.radial = tune(svm, high_mpg~., kernal="radial", data=Auto, ranges=cost_and_gamma)
summary(tune.out.radial)

# We achived best preformance for C = 50, gamma = 0.005 with a CV mse=0.01538462
# Lastly we compute the 3 best fits.

svm.linear = svm(high_mpg~., kernal="linear", cost=70, data=Auto)
svm.poly = svm(high_mpg~., kernal="polynomial", cost=100, degree = 2, data=Auto)
svm.radial = svm(high_mpg~., kernal="radial", cost=50, gamma=0.005, data=Auto)

plotFeatures = function(fit) {
  for(name in names(Auto)) {
    if(!name %in% c("mpg", "high_mpg", "name")) {
      plot(fit, Auto, as.formula(paste("mpg~", name, sep=""))) 
    }
  }
}

plotFeatures(svm.linear)
plotFeatures(svm.poly)
plotFeatures(svm.radial)
