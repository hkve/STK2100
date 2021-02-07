data = read.table("data/yesterday.dat", header=T)
library(ggplot2)

y = c(data$y.yesterday, data$y.tomorrow)
x = c(data$x, data$x)

train_and_test = data.frame(x, y)

fit1 = lm(y~poly(x, degree=5), train_and_test)

train_and_test$x
sorted_idx = order(train_and_test$x)

plot(train_and_test$x, train_and_test$y)
lines(predict(fit1)[sorted_idx]~train_and_test$x[sorted_idx])

MSE1 = mean(fit1$residuals^2)

fit2 = lm(y.yesterday~poly(x,20), data=data)
MSE2 = mean((predict(fit2)-data$y.tomorrow)^2)

maxp = 10
MSE_LOOCV = seq(0,0,length.out=maxp)

for(p in 1:maxp) {
  for(i in 1:nrow(data)) {
      fit = lm(y.yesterday[-i]~poly(x[-i], p), data=data)
      newdata <- data.frame('x'=data$x[i])
      y_pred = predict(fit, newdata)
      print(y_pred)
      break
  }
  break
}
