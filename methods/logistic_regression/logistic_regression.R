data <- read.table("data/processed.cleveland.data", sep=",")

colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)

data[data=="?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"

data$sex = as.factor(data$sex)
data$cp = as.factor(data$cp)
data$fbs = as.factor(data$fbs)
data$restecg = as.factor(data$restecg)
data$exang = as.factor(data$exang)
data$slope = as.factor(data$slope)

data$ca = integer(data$ca)
data$ca = as.factor(data$ca)

data$hd <- ifelse(test=data$hd==0, yes="Healthy", no="Unhealthy")
data$hd = as.factor(data$hd)

str(data)

data = data[!(is.na(data$ca) | is.na(data$thal)),]

fit1 = glm(hd~sex, data=data, family="binomial")
fit2 = glm(hd~., data=data, family = "binomial")

predict.data = data.frame(
  probability.of.hd = fit2$fitted.values,
  hd = data$hd
)

predicted.data = predict.data[order(predict.data$probability.of.hd, decreasing = FALSE),]
predicted.data$rank = 1:nrow(predict.data)

library(ggplot2)
library(cowplot)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) + 
  xlab("Index") + 
  ylab("Predicted prop of getting heart disease")
