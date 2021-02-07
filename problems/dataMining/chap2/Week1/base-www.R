#
# Programs used for graphics and tables of the book:
#    "Data anlysis and data mining" by A.Azzalini and B.Scarpa,
#     © Oxford University Press, 2012 (ISBN 978-0-19-976710-6).
#
# Code of general utility  (© 2003, 2004, 2012 A.Azzalini and B.Scarpa)
#------------------------------------------------------------------------
rm(list=ls())
options(digits=4)
#
par(mfrow=c(1,1))
par(mar=c(3.5, 3.5, 1, 1)+0.1) 
par(mgp=c(2,0.5,0))
#    
col1 <-  "red"
col2 <-  "green"
col3 <-  "blue"
col4 <-  "purple"
col11 <-  "lightblue"
col12 <-  "yellow"

Col<- rep(c(col1,col2,col3,col4),10)
#
lty0 <- 1
lty1 <- 2
lty2 <- 2
lty3 <- 3
Lty <- rep(c(lty0, lty1,lty2,lty3), 10)
#
pch1 <-  1
pch2 <-  2
pch3 <-  5
pch4 <-  6
Pch<- rep(c(pch1,pch2,pch3,pch4),10)
#
cex0 <- 0.8

pred.square <- function(model, x1,x2, grid=50, Z.flag=FALSE)
{
  u<- pretty(x1,n=10)
  x <- seq(u[1], u[10], length=grid)
  u<- pretty(x2,n=10)
  y <- seq(u[1], u[10], length=grid)
  nx <- length(x)
  ny <- length(y)
  xoy <- cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
  X <- matrix(xoy, nx * ny, 2, byrow = FALSE)
  if(Z.flag) pred <- predict(model,  newdata=data.frame(z1=X[,1], z2=X[,2]))
  else  pred <- predict(model,  newdata=data.frame(x1=X[,1], x2=X[,2]))
  list(pred=pred, x=x,y=y, X=X)
}


pause <- function(nome="") {
   cat(nome," ...click <enter> to continue")
   readline()
   invisible()
}

#-------------
convex.hull <- function(x, eval.points){
        ngrid <- nrow(eval.points)
        hull.points <- x[order(x[, 1], x[, 2]), ]
        dh <- diff(hull.points)
        hull.points <- hull.points[c(TRUE, !((dh[, 1] == 0) & (dh[, 2] == 0))), ]
        hull.points <- hull.points[chull(hull.points), ]
        nh <- nrow(hull.points)
        gstep <- matrix(rep(eval.points[2, ] - eval.points[1, ], nh),
                        ncol = 2, byrow = TRUE)
        hp.start <- matrix(rep(eval.points[1, ], nh), ncol = 2,
            byrow = TRUE)
        hull.points <- hp.start + gstep * round((hull.points -
            hp.start)/gstep)
        hull.points <- hull.points[chull(hull.points), ]
        grid.points <- cbind(rep(eval.points[, 1], ngrid), rep(eval.points[,
            2], rep(ngrid, ngrid)))
        D <- diff(rbind(hull.points, hull.points[1, ]))
        temp <- D[, 1]
        D[, 1] <- D[, 2]
        D[, 2] <- (-temp)
        C <- as.vector((hull.points * D) %*% rep(1, 2))
        C <- matrix(rep(C, ngrid^2), nrow = ngrid^2, byrow = TRUE)
        D <- t(D)
        wy <- ((grid.points %*% D) >= C)
        wy <- apply(wy, 1, all)
        wy[wy] <- 1
        wy[!wy] <- NA
        wy <- matrix(wy, ncol = ngrid)
        wy
        }

#-------------

lift.roc <- function(previsti, g, type="bin", plot.it=TRUE)
{
 library(sm)
 if(!is.numeric(g)) stop("g not numeric")
 ind <- rev(order(previsti))
 n <- length(g)
 x1 <-  (1:n)/n
 x2 <- cumsum(g[ind])/(mean(g)*(1:n))
 if(type=="crude" & plot.it) 
   plot(x1, x2, type="l",col=col1,
      xlab="frazione di soggetti previsti", ylab="lift")
 if(type=="sm") {
   a<- sm.regression(x1, x2, h=0.1, display="none")
   if(plot.it)
      plot(a$eval, a$estimate, type="l",xlim=c(0,1), col=col1,
      xlab="frazione di soggetti previsti", ylab="lift")
   }
 if(type=="bin") {
    b <-  binning(x1,x2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    if(plot.it) plot(x, c(x2[1],b$means,1), type="b", xlim=c(0,1),
         ylim=c(1,max(x2)), cex=0.75, col=col1,
         xlab="frazione di soggetti previsti",
         ylab="fattore di miglioramento")
    x1<- x
    x2<- c(x2[1],b$means,1)
   }
 if(plot.it) pause("premere invio")
 u1<- cumsum(1-g[ind])/sum(1-g)
 u2<- cumsum(g[ind])/sum(g)
 if(type=="crude" & plot.it)
   plot(u1, u2, type="l", xlim=c(0,1), ylim=c(0,1), col=col1,
      xlab="1-specificity", ylab="sensibility")
 if(type=="sm") {
     # browser()
     eps<- 0.00001
     a<- sm.regression(u1,log((u2+eps)/(1-u2+2*eps)), h=0.1, display="none")
     q<- exp(a$estimate)/(1+exp(a$estimate))
     if(plot.it) plot(a$eval, q, type="l", xlim=c(0,1), ylim=c(0,1),
       xlab="1-specificity", ylab="sensibility", col=col1)
    }
  if(type=="bin") {
    b <- binning(u1,u2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    y<- c(0,b$means,1)
    if(plot.it)
         plot(x, y, type="b", xlim=c(0,1),
         ylim=c(0,1),cex=0.75, xlab="1-specificity",
         ylab="sensibility", col=col1)
    u1<- x
    u2<- y
   }                      
 if(plot.it) {
   abline(0,1, lty=2, col=col2)
 }
 invisible(list(x1,x2,u1,u2))
}

#------------

matrice.confusione <- function(previsti, osservati){
  a <-  table(previsti, osservati)
  err.tot <- 1-sum(diag(a))/sum(a)
  fn <- a[1,2]/(a[1,2]+a[2,2])
  fp <- a[2,1]/(a[1,1]+a[2,1])
  print(a)
  cat("total error: ", format(err.tot),"\n")
  cat("false positives/negatives: ",format(c(fp, fn)),"\n")
  invisible(a)
}


#------------
detach.all <- function(){
  lista<- search()
  n<- length(lista)
  for(i in (n-1):2){
    if(substr(lista[i],1,8) != "package:"   &
       substr(lista[i],1,9) != "Autoloads" &
       substr(lista[i],1,6) != "tools:" )
       detach(pos=i)
  }
  invisible()
}



