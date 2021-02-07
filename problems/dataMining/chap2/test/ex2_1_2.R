auto = read.table("data/auto.dat", header=TRUE)

attach(auto)

fuel1 = factor(fuel, c("gas", "diesel"))
fitCubic <- lm(city.distance~engine.size+I(engine.size^2)+I(engine.size^3)+fuel1, data=auto)
fitQuad = lm(city.distance~engine.size+I(engine.size^2)+fuel1, data=auto)

beta <- array(coef(fitCubic))

print(length(engine.size))
plot(engine.size, beta[1]+beta[2]*engine.size+beta[3]*engine.size^2+beta[4]*engine.size^3+beta[5])
plot(engine.size, beta[1]+beta[2]*engine.size+beta[3]*engine.size^2+beta[4]*engine.size^3)
