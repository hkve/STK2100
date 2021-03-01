
# Make data with 1000 points and 20 features
set.seed(42)
n = 1000
p = 20
X = matrix(rnorm(p*n), n, p)

# Make random coefs
b = rnorm(p)
# Set some equal to zero
b[1]=b[4]=b[14]=b[12] =0

# Target
y = X %*% b + rnorm(n)

# Train/test split
train_size = 0.9
train = sample(seq(1,n), train_size*n, replace=FALSE) 

X.train = X[train,]
X.test = X[-train,]
y.train = y[train,]
y.test = y[-train,]

data.train = data.frame(x = X.train, y = y.train)
data.test = data.frame(x = X.test, y = y.test)

library(leaps)

# Preform best subset selection
best_subset_selection = regsubsets(y~., data=data.train, nvmax=p)

train.matrix = model.matrix(y~., data=data.train)
test.matrix = model.matrix(y~., data=data.test)

# Calculate train and test MSE
MSE.train = seq(0,0, length.out = p)
MSE.test= seq(0,0, length.out = p)

for( i in 1:p ) {
  coefs = coef(best_subset_selection, id=i)
  y_pred.train = train.matrix[, names(coefs)] %*% coefs
  y_pred.test = test.matrix[, names(coefs)] %*% coefs 
  
  MSE.train[i] = sum((data.train["y"] - y_pred.train)^2)/nrow(data.train)
  MSE.test[i] = sum((data.test["y"]- y_pred.test)^2)/nrow(data.test)
}

library(ggplot2)
MSE = data.frame(train = MSE.train, test=MSE.test)

# Plot them

plot(MSE.train, xlab="Number of explanatory variables", ylab="MSE", type="l", col="red")
par(new=TRUE)
plot(MSE.test, type="l", col="blue", xlab="", ylab="")
legend(1,95, legend=c("Train", "Test"), col=c("red", "blue"))

best_model.train = which.min(MSE.train)
best_model.test = which.min(MSE.test)


# b.errors = seq(0,0, length.out = p)
# library(stringr)
# for( i in 1:p ) {
#   coefs = coef(best_subset_selection, id=i)
#   coefs.names = names(coefs[-1])
#   
#   c = seq(0,0, length.out = length(coefs.names))
#   for( j in 1:length(coefs.names)) {
#     coef.idx = strtoi(str_remove(coefs.names[j], "x."))  
#     c[j] <-coef.idx
#   }
#   
#   print(coef.idx)
# }

