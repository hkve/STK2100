nuclear = read.table("nuclear.dat", sep="\t", header=T)
attach(nuclear)

# These are binary variables (0 or 1) so set them as factors 
nuclear$pr = as.factor(nuclear$pr)
nuclear$ne = as.factor(nuclear$ne)
nuclear$ct = as.factor(nuclear$ct)
nuclear$bw = as.factor(nuclear$bw)
nuclear$pt = as.factor(nuclear$pt)


all.fit = lm(log(cost)~.-t1-bw-t2-pr-cum.n-ct, data=nuclear)
summary(all.fit)

nuclear_backwards_sub <- data.frame(nuclear)
for (i in 1:ncol(nuclear)) {
    fit <- lm(log(cost)~., data=nuclear_backwards_sub)
    p_vals <- summary(fit)$coefficients[-1,4]
    max_idx <- as.integer(which.max(p_vals)) 
    
    print(p_vals)
    print(p_vals[max_idx])
    print(max_idx)
        
    if(p_vals[max_idx] < 0.05) {
      break
    } 
    else {
      nuclear_backwards_sub <- nuclear_backwards_sub[,-(max_idx+1)]
    }
}



