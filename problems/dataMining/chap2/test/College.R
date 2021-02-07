College = read.csv("College.csv")
row.names(College)=College[,1]
College = College[,-1]

plot(College$Private, College$Outstate)

Elite=rep("No", nrow(College))
Elite[College$Top10perc > 50] <-"Yes"
Elite = as.factor(Elite)
College=data.frame(College,Elite)

summary(Elite)
par(mfrow=c(1,2))
plot(College$Private, College$Outstate, xlab="Private",ylab="Number of out of state students", main="All colleges")
plot(Elite, College$Outstate, ylab="", xlab="Private", main="'Elite' colleges")

summary(College)
par(mfrow=c(2,2))
hist(Apps, breaks = 50)
hist(Accept, breaks = 50)
hist(Enroll, breaks = 50)
hist(F.Undergrad, breaks=50)

# Apply/Accept for Normal vs elite 
par(mfrow=c(2,2))
hist(Apps[Elite=="Yes"], main="Elite", xlab="Apps", breaks=30)
hist(Apps[Elite=="No"], main="Normal", xlab ="Apps", breaks=30)

hist(Accept[Elite=="Yes"], main="", xlab="Accepts", breaks=30)
hist(Accept[Elite=="No"], main="", xlab="Accepts", breaks=30)
