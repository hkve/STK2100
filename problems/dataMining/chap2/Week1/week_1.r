#-------------------------------------------------------------------------------------------------------------
# 2.1

# Set the working directory:
# Session -> Set Working Directory -> To Source File Location
# Assume that all files are located in the same folders as the
# file that you currently have open.
auto <- read.table("auto.dat", header=TRUE)

# Or read from the webpage
auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)

# Take a look at the data
summary(auto)
head(auto)
bodystyle = auto$bodystyle
bodystyle
attach(auto)

# Convert fuel into a categorical explanatory variable
fuel1 <- factor(fuel, levels = c("gas", "diesel"))

# See how to use lm() function
help(lm)
# Can also use
?lm

# Fit the original linear model in (2.14) and look at the fitted coefficients
fit_orig <- lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel1)
summary(fit_orig)

# Do the same, but removed the quadratic term
fit_new <- lm(city.distance ~ engine.size + I(engine.size^2) +  fuel1)
print(summary(fit_new))

# We see that the both the estimate and standard error of (engine size)^2 has decreased,
# but still resulting in a smaller p-value. 
# This indicates that the two variables were both used to estimate the same information.

# This is clear if we plot the cubic and quadratic terms together. 
# We see a strong relation between them. 
plot(I(engine.size^2),  I(engine.size^3))
cor(I(engine.size^2),  I(engine.size^3))


#-------------------------------------------------------------------------------------------------------------
# 2.2
# Create values between 1 and 7, inclusive.
x = seq(1, 7, 0.1)

# Get the indices of the gas engines
idx = fuel1 == 'gas'

# Plot the engine size vs city distance
plot(engine.size[idx], city.distance[idx], xlim = c(1, 7), ylim = c(0, 22),
     xlab = "Engine Size", ylab = "City Distance", main = "Gasoline Cars")

# Get the coefficients of the model fitted in the previous exercise
beta <- coef(fit_orig)

# Add the fitted linear model (linear in model parameters, the plotted curve is not linear) 
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 2, lty = 1, lwd = 2)

# The extrapolation doesn't seem hopeless, though it's hard to say if it is reasonable.
# As explained in the book, the two-cylinder cars behave differently.
# Also, we have some problems estimating for low engine size.

# --- another way to get predicted values -----
# Do this by calling the predict function, but then the input data
# needs to be a data.frame for the function to work.
help(predict.lm)
only_gas = factor(rep('gas', length(x)), levels=levels(fuel1))
new_x = data.frame(x, only_gas)
colnames(new_x) = c("engine.size", "fuel1")
head(new_x)
preds = predict(fit_orig, new_x)
preds2 = beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3
all(preds == preds2)


#-------------------------------------------------------------------------------------------------------------
# 2.3

# Define the response variable, consumption.
y = 1/city.distance

# Fit the linear model given in (2.17) and we get R-squared = 0.6381 \approx 0.64
fit_17 <- lm(y ~ engine.size + fuel1)
summary(fit_17)$r.squared 

# See equation (2.15). 
# R^2 = 1 - (residual deviance)/(total deviance)
# Or R^2 = 1 - RSS / TSS to use the notation from STK1110.
# We get R-squared = 0.5615 \approx 0.56
r2_17 <-  1 - sum((city.distance - 1/fitted(fit_17))^2) / ((length(city.distance)-1)*var(city.distance))
r2_17 

# We are performing a non-linear transformation of our targets,
# so we expect R^2 to be different.
# To compare the models we need to do this in the same space (of our response),
# meaning that (2.14) is still a better fit as it
# obtained R-squared = 0.597 \approx 0.6, which is higher than 0.56.


#-------------------------------------------------------------------------------------------------------------
# ISL 1:

# For each of parts (a) through (d), indicate whether we would generally expect
# the performance of a flexible statistical learning method to be better or
# worse than an inflexible method. Justify your answer.

# (a)
# Q: The sample size n is extremely large, and the number of predictors p is small.
# A: better - a more flexible approach will fit the data closer and with the
#    large sample size a better fit than an inflexible approach would be obtained

# (b)
# Q: The number of predictors p is extremely large, and the number of observations n is small.
# A: worse - a flexible method would overfit the small number of observations

# (c)
# Q: The relationship between the predictors and response is highly non-linear.
# A: better - with more degrees of freedom, a flexible model would obtain a
#    better fit

# (d)
# Q: The variance of the error terms, i.e. \sigma^2 = Var(\epsilon), is extremely high.
# A: worse - flexible methods fit to the noise in the error terms and increase variance

#-------------------------------------------------------------------------------------------------------------
# ISL 2:

# Explain whether each scenario is a classification or regression problem,
# and indicate whether we are most interested in inference or prediction.
# Finally, provide n and p.

# (a)
# Q: We collect a set of data on the top 500 firms in the US. For each firm we
#    record profit, number of employees, industry and the CEO salary.
#    We are interested in understanding which factors affect CEO salary.
# A: Regression and inference. Quantitative output of CEO salary based on CEO
#    firm's features. 
#    n - 500 firms in the US,
#    p - profit, number of employees, industry

# (b)
# Q: We are considering launching a new product and wish to know whether it will
#    be a success or a failure. We collect data on 20 similar products that were
#    previously launched. For each prod- uct we have recorded whether it was a
#    success or failure, price charged for the product, marketing budget,
#    competition price, and ten other variables.
# A: Classification and prediction. Predicting new product's success or failure.
#    n - 20 similar products previously launched
#    p - price charged, marketing budget, comp. price, ten other variables
 
# (c)
# Q: We are interested in predicting the % change in the USD/Euro exchange rate
#    in relation to the weekly changes in the world stock markets. Hence we
#    collect weekly data for all of 2012. For each week we record the % change
#    in the USD/Euro, the % change in the US market, the % change in the
#    British market, and the % change in the German market.
# A: Regression and prediction. Quantitative output of % change
#    n - 52 weeks of 2012 weekly data
#    p - % change in US market, % change in British market, % change in German market

#-------------------------------------------------------------------------------------------------------------
# ISL 8
# Download package that contains all the data used in 
# An Introduction to Statistical Learning: with Applications in R
install.packages("ISLR")
library("ISLR")
attach(College)
data(College)

# Or we can directly download it from the webpage 
College <- read.csv("https://statlearning.com/College.csv", header = TRUE)
summary(College)

# Or load it from the computer (if you have it in the same folder as this file)
College = read.csv(paste(getwd(), "/College.csv", sep = ""))
summary(College)

# 8. (b) 
# If we used the first method to get College data, then we can skip this.
fix(College)
rownames(College) = College[,1]
College = College[,-1]
fix(College)

# 8. (c)
# i.
summary(College)

# ii.
# To make it work I needed to manually set some of the features as categorical.
# This was not necessary before, maybe an update in R. I do not know.
# Not needed if use ISLR library method.
College$Private = factor(College$Private)
pairs(College[,1:10])

# iii.
plot(College$Private, College$Outstate)

# iv.
Elite = rep("No", nrow(College))
Elite[College$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
College = data.frame(College, Elite)
summary(College$Elite)
plot(College$Elite, College$Outstate)

# v.
par(mfrow=c(2,2))
hist(College$Apps)
hist(College$perc.alumni, col=2)
hist(College$S.F.Ratio, col=3, breaks=10)
hist(College$Expend, breaks=100)

# vi.
par(mfrow=c(1,1))
# High tuition correlates to high graduation rate.
plot(College$Outstate, College$Grad.Rate)

# Colleges with low acceptance rate tend to have low S:F ratio.
plot(College$Accept / College$Apps, College$S.F.Ratio)

# Colleges with the most students from top 10% perc don't necessarily have
# the highest graduation rate. Also, rate > 100 is erroneous!
plot(College$Top10perc, College$Grad.Rate)


