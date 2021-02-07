auto = read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header=TRUE)

fuel1 = factor(auto$fuel, levels=c("gas", "diesel"))

y = 1/auto$city.distance

fit1 = lm(y~ auto$engine.size + fuel1)
print(summary(fit1)$r.squared)

x <- auto$engine.size
fit2 = lm(auto$city.distance ~ poly(x, degree = 3) + fuel1)

RSS1 = sum((auto$city.distance-1/fitted(fit1))^2)
TSS1 = sum((auto$city.distance-mean(auto$city.distance))^2)
R2_1 = 1 - RSS1/TSS1

R2_2 = summary(fit2)$r.squared 

print(c(R2_1, R2_2))
print(cor(auto$city.distance, auto$engine.size))
