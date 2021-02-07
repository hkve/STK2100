library(ggplot2)

auto = read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header=TRUE)
fuel1 = factor(auto$fuel, levels=c("gas", "diesel"))
cylinder2 = factor(auto$n.cylinders==2)

#fitPoly = lm(log(highway.distance)~ log(engine.size) +fuel1 + cylinder2 + log(curb.weight), data=auto)
fit1 = lm(highway.distance~city.distance+factor(brand), data=auto)
summary(fit1)
# x = seq(min(auto$engine.size), max(auto$engine.size), length.out=203)
# 
# fitDf  = data.frame(
#   engine.size = x
# ) 
# fitDf$highway.distance = predict(fitPoly, fitDf)
# 
# 
# ggplot(auto, aes(x=engine.size, y=highway.distance)) +  
#     geom_point() +
#     geom_line(data=fitDf, aes(x = engine.size, y = highway.distance)) 
    

