# Backfitting approach using simple linear regression

set.seed(42)

n = 100
x1 = rnorm(n)
x2 = rnorm(n)

y = 0.33*x1 - 2.1*x2 + rnorm(n) + 1.2

# Random guess
beta1 = 4

iter = 10
betas = matrix(data=0, nrow=iter, ncol=3)

for(i in 1:iter) {
   a = y-beta1*x1
   beta2 = lm(a~x2)$coef[2]
   
   a = y-beta2*x2
   temp = lm(a~x1)$coef
   beta0 = temp[1]
   beta1 = temp[2]
   
   betas[i,] = c(beta0, beta1, beta2)
}

cols = rainbow(3)
par(mfrow=c(1,3))
for(i in 1:3) {
  plot(betas[,i], col=cols[i], xlab="iteration", ylab=i-1, type="l") 
  par(new=TRUE)
}

sprintf("Final, beta0: %.2f, beta1: %.2f, beta2: %.2f", beta0, beta1, beta2)
