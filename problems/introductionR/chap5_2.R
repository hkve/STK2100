n = seq(1, 1e6, length.out = 1e6)
p = 1-(1-1/n)^n

plot(n,p, type="l", ylim=c(0,1))

