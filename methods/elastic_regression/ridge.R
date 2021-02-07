library(glmnet)

set.seed(42)
n = 1000
p = 100
real_p = 15

X = matrix(rnorm(n*p, mean=2, sd=1), nrow=n, ncol=p)
y = apply(X[,1:real_p], 1, sum) + rnorm(n)

# First we need to scale the data
scale = function(X) {
  x_means = apply(X, 2, mean)
  x_stds = apply(X, 2, sd)      
  for (i in 1:nrow(X)) {
    for (j in 1:ncol(X)) {
        X[i,j] = (X[i,j]-x_means[j])/x_stds[j]
    }
  }
  X
}

X = scale(X)

train_rows = sample(1:n, .66*n)
X.train = X[train_rows,]
X.test = X[-train_rows,]

y.train = y[train_rows]
y.test = y[-train_rows]

ridge.fit = cv.glmnet(X.train, y.train, type.measure = "mse", alpha=0, family="gaussian")
ridge.predict = predict(ridge.fit, s=ridge.fit$lambda.1se, newx=X.test)

mean((y.test-ridge.predict)^2)
