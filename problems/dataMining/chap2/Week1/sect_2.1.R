# Programs used for graphics and tables of the book:
#    "Data anlysis and data mining" by A.Azzalini and B.Scarpa,
#    © Oxford University Press, 2012 (ISBN 978-0-19-976710-6).
#
# Code regarding section 2.1 (© 2003, 2004, 2012 A.Azzalini and B.Scarpa)
#------------------------------------------------------------------------

# Set the working directory:
# Session -> Set Working Directory -> To Source File Location
# Assume that all files are located in the same folders as the
# file that you currently have open.

source("base-www.R")
auto<- read.table("auto.dat", header=TRUE)
attach(auto)

#
name <-  "figure 2.1"
#
n <- nrow(auto)
#
d <- fuel=="gas"
pairs(auto[,
  c("city.distance", "engine.size","n.cylinders","curb.weight")],     
  labels=c("City\ndistance", "Engine\nsize","Number of\ncylinders",
         "Curb\nweight"),
  col=ifelse(d,col1,col2), pch=ifelse(d,pch1,pch2),
  cex=10/sqrt(n)  )
pause(name)
#---------------------------------------
#
name <- "figure 2.2"
#
plot(engine.size, city.distance, type="n",
     ylab="City distance (km/L)",
     xlab="Engine size (L)", xlim=c(1,5.5))
d <- fuel=="gas"
points(engine.size[d], city.distance[d], col=col1, pch=pch1)
points(engine.size[!d], city.distance[!d], col=col2, pch=pch2)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("Gasoline  ","Diesel"))
#
pause(name)
#---------------------------------------
name <- "table 2.1"
fuel1 <- factor(fuel, levels=c("gas","diesel"))
fit3 <- lm(city.distance~ engine.size + I(engine.size^2)+
            I(engine.size^3)+ fuel1)
print(summary(fit3))
pause(name)

#
name <- "figure 2.3" 
plot(engine.size, city.distance, type="n",
     ylab="City distance",
     xlab="Engine size",  xlim=c(1,5.5))
d <- fuel=="gas"
points(engine.size[d], (city.distance[d]), col=col1, pch=pch1)
points(engine.size[!d], (city.distance[!d]), col=col2, pch=pch2)
#
x <- (seq(min(engine.size), max(engine.size), length=200))
x <- seq(1,5.5,  length=200)
beta<- coef(fit3)
lines(x, beta[1]+ beta[2]*x+beta[3]*x^2+beta[4]*x^3, col=col2, lty=lty1)
lines(x,  beta[1]+ beta[2]*x+beta[3]*x^2+beta[4]*x^3+beta[5],
       col=col1, lty=lty1)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("Gasoline","Diesel"))
pause(name)
#
name <- "figure 2.4(a)" 
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit3, which=1, sub.caption="", add.smooth=FALSE )
#plot(fit3, which=1, sub.caption="", add.smooth=FALSE)
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name <- "figure 2.4(b)" 
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit3, which=2, sub.caption="")
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
#---------------------------------------
name <- "figure 2.5" 
plot(engine.size, 1/(city.distance), type="n",
     ylab="Consumption",
     xlab="Engine size")
d <- fuel=="gas"
points(engine.size[d], 1/(city.distance[d]), col=col1, pch=pch1)
points(engine.size[!d], 1/(city.distance[!d]), col=col2, pch=pch2)
#
fit2 <- lm(1/(city.distance)~ engine.size+ fuel1)
beta<- coef(fit2)
abline(beta[1]+beta[3], sum(beta[2]) , col=col2, lty=lty1)
abline(beta[1:2], col=col1, lty=lty1)
pause(name)
#
name <- "table 2.2"
print(summary(fit2))
pause(name)
#
name <- "figure 2.6"
plot(engine.size, city.distance, type="n",
     ylab="City distance",
     xlab="Engine size",  xlim=c(1,5.5))
d <- fuel=="gas"
points(engine.size[d], (city.distance[d]), col=col1, pch=pch1)
points(engine.size[!d], (city.distance[!d]), col=col2, pch=pch2)
#
x <- (seq(min(engine.size), max(engine.size), length=200))
x <- seq(1,5.5,  length=200)
lines(x, 1/(beta[1]+ beta[2]*x), col=col1, lty=lty1)
lines(x, 1/(beta[1]+beta[3]+ beta[2]*x),  col=col2, lty=lty1)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("Gasoline","Diesel"))
pause(name)
#
r2.2 <- 1-sum((city.distance-1/(fitted(fit2)))^2) / ((n-1)*var(city.distance))
cat("R squared=",r2.2,"\n")

fit2a <- update(fit2, . ~. + factor(n.cylinders==2), data=auto)
fit2b <- update(fit2a, . ~. + curb.weight, data=auto)
#
name <- "figure 2.7(a)" 
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit2, which=1, sub.caption="", add.smooth=FALSE)
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name <- "figure 2.7(b)" 
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit2, which=2, sub.caption="")
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
#---------------------------------------
#
name <- "table 2.3"
fit1<- lm(log(city.distance)~ log(engine.size)+ fuel1)
print(summary(fit1))
pause(name)

#
name <- "figure 2.8(a)"
#
plot(log(engine.size), log(city.distance), type="n",
     ylab="log(city distance)",
     xlab="log(engine size)")
d <- fuel=="gas"
points(log(engine.size[d]), log(city.distance[d]), col=col1, pch=pch1)
points(log(engine.size[!d]), log(city.distance[!d]), col=col2, pch=pch2)
beta<- coef(fit1)
abline(beta[1:2], col=col1, lty=lty1)
abline(beta[1]+beta[3], sum(beta[2]) , col=col2, lty=lty1)
pause(name)

#----
name <- "figure 2.8(b)"
plot(engine.size, city.distance, type="n",
     ylab="City distance (km/L)",
     xlab="Engine size (L)",  xlim=c(1,5.5))
d <- fuel=="gas"
points(engine.size[d], city.distance[d], col=col1, pch=pch1)
points(engine.size[!d], city.distance[!d], col=col2, pch=pch2)
x <- log(seq(min(engine.size), max(engine.size), length=200))
x <- log(seq(1,5.5,  length=200))
beta<- coef(fit1)
lines(exp(x), exp(beta[1]+ beta[2]*x), col=col1, lty=lty1)
lines(exp(x), exp(beta[1]+beta[3]+ beta[2]*x),  col=col2, lty=lty1)
legend(4.5, 20, pch=c(pch1, pch2), col=c(col1,col2),
       legend=c("Gasoline","Diesel"))
pause(name)

#
name <- "figure 2.9(a)"
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit1, which=1, sub.caption="",  add.smooth=FALSE)
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name <- "figura 2.9(b)"
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit1, which=2, sub.caption="")
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name<- "table 2.4"
r2.1 <- 1-sum((city.distance-exp(fitted(fit1)))^2)/
         ((n-1)*var(city.distance))
cat("R squared=",r2.1,"\n")
I.D   <- factor(n.cylinders==2, labels=c(">2","=2"))
fit1a <- update(fit1, . ~. + I.D, data=auto)
fit1b <- update(fit1a, . ~. + log(curb.weight), data=auto)
fit1c <- update(fit1, . ~. + log(curb.weight), data=auto)

print(summary(fit1b))
r2.1b <-  1-sum((city.distance-exp(fitted(fit1b)))^2)/
             (202*var(city.distance))
cat("R squared=",r2.1b,"\n")

pause(name)
#
name<-"figure 2.10(a)"
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit1b,1, sub.caption="",  add.smooth=FALSE)
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name<-"figure 2.10(b)"
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit1b,2, sub.caption="")
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name<-"figure 2.10(c)"
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit1b,3, sub.caption="",  add.smooth=FALSE)
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#
name<-"figure 2.10(d)"
par(mar=c(3.5, 3.5, 1.2, 1)+0.1)
plot(fit1b,4, sub.caption="")
pause(name)

par(mar=c(3.5, 3.5, 1, 1)+0.1)
#

detach.all()

