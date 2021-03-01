
# Create test data
set.seed(42)
n = 100
X = rnorm(n)
epsilon = rnorm(n)

# Actual model coefs
b_true = c(1,1,1,0.25)

# Predictions with noise
Y = b_true[1] + b_true[2]*X + b_true[3]*X^2 + b_true[4]*X^3 + epsilon

X.plot_vals = seq(min(X), max(X), by=0.01)
Y.plot_vals = b_true[1] + b_true[2]*X.plot_vals + b_true[3]*X.plot_vals^2 + b_true[4]*X.plot_vals^3

# Plot the data and true model
plot(X, Y)
lines(X.plot_vals, Y.plot_vals, col="red", lw=2)

library(leaps)
# regsubsets needs df
data = data.frame(X, Y)


model.full = regsubsets(Y~ poly(X, 10, raw=T), data=data, nvmax=10)
model.summary = summary(model.full)
ls(model.summary)

cp = which.min(model.summary$cp)
bic = which.min(model.summary$bic)
adjr2 = which.max(model.summary$adjr2)

plot(model.summary$bic)

sprintf("Number of params in best model from cp: {%i}, bic: {%i}, adjr2: {%i}", cp, bic, adjr2)
