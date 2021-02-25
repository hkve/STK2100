data = read.csv("https://www.uio.no/studier/emner/matnat/math/STK2100/v21/data/edu_bodyfat.csv", header = TRUE, row.names = 1)

library(MASS)

model.full = lm(pcfat~., data=data)
model.empty = lm(pcfat~1, data=data)  

model.forward = stepAIC(model.full, k=2, direction="backward")
model.backward= stepAIC(model.empty, k=2, direction="forward", scope=list(lower=model.empty, upper=model.full))

model.forward$anova
# pcfat ~ age + weight + neck + ab + hip + thigh + forearm + wrist

model.backward$anova
# pcfat ~ ab + weight + wrist + forearm + neck + age + thigh +hip

# Both selections end up with the same model. This is not surprising at there is nothing against this happening. 
# Both model ended with the lowest AIC (the reason for the equal stop), but this is not necessarily always true. 
# If the forward selection had 3 variables and non of the four gave an improvement, it could come at a stand still,
# Even though the true lowest AIC is at say 8 variables. The same is true for backward selection (only, you know, backwards).