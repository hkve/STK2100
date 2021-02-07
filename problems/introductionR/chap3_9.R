library("ISLR")
attach(Auto)

pairs(Auto)

# Removes name coulumn
#quantitative = subset(Auto, select=-c(name))
quantitative = Auto[, names(Auto)!=c("name")]

# Get corrolation between each variable p
cor(quantitative)

fit1 = lm(mpg~ ., data=quantitative)
summary(fit1)

# weight, year and origin are the most signif variables, possible displacement. 
# The year coef is positive, meaning newer cars use more fuel. This sounds strange but the range is just from 71 to 80.

plot(fit1)
# Residuals seems to increase for higher responses
# QQ plot confirms this, heavy on right tail
# Scale-location shows that the sqrt of std residuals are pretty dense, but some outliers
# Residuals vs Leverage plot, mr 14 is def outlier. 327 and 394 have high std residuals, but lower leverage

quantitative = quantitative[-c(14, 327, 394),]
fit2 = lm(mpg~year+factor(origin)+weight*horsepower, data=quantitative)
summary(fit2)

fit3 = lm(log(mpg)~year+factor(origin)+weight*horsepower, data=quantitative)
summary(fit3)

transformed_R2 = function(fit, y, reverse_trans) {
  y_predict = reverse_trans(predict(fit))
  RSS = sum((y-y_predict)^2)
  TSS = sum((y-mean(y))^2)
  1 - (RSS/TSS)
}

transformed_R2(fit3, quantitative$mpg, exp)

