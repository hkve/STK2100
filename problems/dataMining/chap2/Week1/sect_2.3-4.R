# Programs used for graphics and tables of the book:
#    "Data anlysis and data mining" by A.Azzalini and B.Scarpa,
#     © Oxford University Press, 2012 (ISBN 978-0-19-976710-6).
#
# Code regarding section 2.3-2.4 (© 2003, 2004, 2012 A.Azzalini and B.Scarpa)
# ------------------------------------------------------------------------
source("base-www.R")
#
brazil <- read.csv("brazil.csv",  row.names=1, header=TRUE)
brazil[,"id"] <- factor(brazil[,"id"])
brazil[,"satisfaction"] <- factor(brazil[,"satisfaction"], ordered=TRUE)
brazil[,"education"] <- factor(brazil[,"education"], ordered=TRUE)
for(k in c(6:9,11:40)) 
          brazil[,k] <- factor(brazil[,k])
brazil[,"ok"] <- factor(brazil[,"satisfaction"]=="3" |  
                   brazil[,"satisfaction"]=="4", labels=c("not.satisf","satif"))
attach(brazil)

satisf <- (satisfaction > 2)
old <- (age > 45)
freq<- table(satisf, old)
name<- "table at p.41"
print(freq)
pause(name)
rel <- t(t(freq)/apply(freq,2,sum))
table(old) # 309   191

# ------------------------------------
# computation of W
#

 p2<- 157/191
 p<- 382/500
 p1 <- 225/309
 225*log(p1)+84*log(1-p1)+157*log(p2)+34*log(1-p2)
# [1] -270.2
 382*log(p)+118*log(1-p)
# [1] -273.2
 225*log(p1)+84*log(1-p1)+157*log(p2)+34*log(1-p2)- (382*log(p)+118*log(1-p))
# [1] 2.964
 1-pchisq(2*2.964,1)
#[1] 0.01490
#-----or:
 m1<- glm(satisf~old, family=binomial)
 summary(m1)
 m1$null.deviance- deviance(m1)
# [1] 5.928
cat("the source file include something else...\n")
# ------------------------------------

#
# Logistic regression of satisfaction with respect to income or age
#
name <- ""
y <- as.numeric(table(satisf, pincome)[2,])
n <- as.numeric(table(pincome))
income <- sort(unique(pincome))
plot(income, y/n, ylim=c(0,1))
plot(income, log((y+0.5)/(n-y+0.5)))
cat("the source file include something more...\n")
#
y <- as.numeric(table(satisf, age)[2,])
n <- as.numeric(table(age))
age.s <- sort(unique(age))
par(mfrow=c(1,2))
plot(age.s, y/n, ylim=c(0,1))
plot(age.s, log((y+0.5)/(n-y+0.5)))
par(mfrow=c(1,1))
pause("(this is useful for exploration of data)")

#
m1 <- glm(cbind(y,n-y)~ age.s, family=binomial)
m2 <- glm(cbind(y,n-y)~ age.s+I(age.s^2), family=binomial)
#
name <- "figure 2.12(a)"
plot(age.s, y/n, ylim=c(0,1), xlim=c(15,70),
     ylab="Pr{ satisfied customers | age }",xlab="age")
x <- seq(15,70, length=200)
lines(x, predict(m2, newdata=data.frame(age.s=x), type="response"),
      col=col2, lty=2)
lines(x, predict(m1, newdata=data.frame(age.s=x), type="response"),
      col=col1, lty=1)
pt <- list(x=45, y=0.4)
legend(pt, legend=c("Linear model", "Quadratic model"),
       lty=c(1, 2), col=c(col1,col2))
pause(name)
#----
name <- "figure 2.12(b)"
plot(age.s, y/n, ylim=c(0,1), xlim=c(15,70),
     ylab="Pr{ satisfied customers | age }", xlab="age", cex=sqrt(n)/6)

x <- seq(15,70, length=200)
lines(x, predict(m2, newdata=data.frame(age.s=x), type="response"),
      col=col2, lty=2)
lines(x, predict(m1, newdata=data.frame(age.s=x), type="response"),
      col=col1, lty=1)
# cat("scegliere un punto\n")
legend(pt, legend=c("Linear model", "Quadratic model"),
       lty=c(1, 2), col=c(col1,col2))
pause(name)

#------
name<- "table 2.5"
print(summary(m2))
print(summary(m1))
pause(name)
#
detach.all()
