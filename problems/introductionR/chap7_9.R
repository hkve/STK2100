library(MASS)
set.seed(42)

fix(Boston)

fit.cubic_poly = lm(nox~poly(dis,3), data=Boston)
summary(fit.cubic_poly)

# High P-values for all coefs, they all seem to be important

x = seq(min(Boston$dis), max(Boston$dis), length.out = 100)
y = predict(fit.cubic_poly, data.frame(dis=x))

plot(Boston$dis, Boston$nox)
lines(x, y, col="red", lw=2)

n_poly = 10
cols = rainbow(n_poly)

{
plot(Boston$dis, Boston$nox)
RSS = seq(0,0, length.out = n_poly)
for(i in 1:n_poly) {
  fit.poly = lm(nox~poly(dis, i), data=Boston)
  
  y = predict(fit.poly, data.frame(dis=x))

  lines(x, y, col=cols[i])    
  RSS[i] = sum(fit.poly$residuals^2)
}

legend(x="topright", legend=1:10, col=cols, lty=c(1,1), bty="n", cex=0.7)
}

plot(RSS, xlab="Poly degree", type="l")
points(which.min(RSS), RSS[which.min(RSS)], col="red", pch=19)

# The most complex model gives highest RSS. This is expected and is highly overfitted

# Cross validation
library(boot)
RSS.CV = seq(0,0, length.out = n_poly)

for(i in 1:n_poly) {
  fit = glm(nox~poly(dis, i), data=Boston)
  CV = cv.glm(Boston, fit, K=10) 
  
  RSS.CV[i] = CV$delta[1]
}

RSS.CV
plot(RSS.CV, xlab = "Poly degree", ylab="CV MSE",type="l")
points(which.min(RSS.CV), RSS.CV[which.min(RSS.CV)], col="red", pch=19)

# 4 seems to be the best fit, but 3 is almost equal. Choose 3 since a simpler model is prefered.

# Splines 
library(splines)

fit.spline = lm(nox~bs(dis, knots=c(4, 7, 10)), data=Boston)
summary(fit.spline)

# All knots seems to be significant.

plot(Boston$dis, Boston$nox)
lines(x, predict(fit.spline, data.frame(dis=x)), col="red", lw=2)

# Different degrees of freedom

df1 = 3
df2 = 16

RSS.SPLINES = seq(0,0, length.out=df2)
cols=rainbow(df2)

plot(Boston$dis, Boston$nox, xlab="Distance", ylab="Nox")

for(i in df1:df2) {
  fit = lm(nox~bs(dis, df=i), data=Boston)
  RSS.SPLINES[i] = sum(fit$residuals^2)
  
  lines(x, predict(fit, data.frame(dis=x)), col=cols[i])
    
}

legend(x="topright", legend=df1:df2, col=cols, lty=c(1,1), bty="n", cex=0.7)

plot(df1:df2,RSS.SPLINES[df1:df2], type="l", xlab="Degrees of freedom", ylab="RSS")
points(which.min(RSS.SPLINES[df1:df2])+df1-1, RSS.SPLINES[which.min(RSS.SPLINES[df1:df2])+df1-1], pch=19, col="red")

# Different degrees of freedom with 10-fold cross-validation

RSS.CV.SPLINES = seq(0,0, length.out = df2)

for(i in df1:df2) {
  fit = glm(nox~bs(dis, df=i), data=Boston)
  fit.cv = cv.glm(Boston, fit, K=10)
  
  RSS.CV.SPLINES[i] = fit.cv$delta[1]
}

plot(df1:df2,RSS.CV.SPLINES[df1:df2], type="l", xlab="CV Degrees of freedom", ylab="MSE")
points(which.min(RSS.CV.SPLINES[df1:df2])+df1-1, RSS.CV.SPLINES[which.min(RSS.CV.SPLINES[df1:df2])+df1-1], pch=19, col="red")

# The CV results support a splines with 10-13 degrees of freedom