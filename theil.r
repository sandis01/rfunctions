theil <- function(x, y, alpha=0.05) {

#  Name..: theil
#  AUthor: sven.sandin@meb.ki.se
#  Date..: 2001-05-14
#
#  06Jul2004
#  1) Added header and example
#  2) Replaced function parameter DFR with the DATA=
#  3) Added checks for the function parameters
#
#  17Jun2008
#  1) Changed function call to theil(data$x,data$y) instead of theil(x,y,data=data)
#
#  Example:
#  --------
#  x <- 1:20
#  y <- 1+2*x+rnorm(x)
#  d<-data.frame(x,y)
#  e<-theil(d,x,y)
#
#  glm(y~x,data=d)
#  cor.test(d$x,d$y,method="kendall")

#-- Logical checks
if (alpha<=0 | alpha>=1) stop('alpha must be between 0 and 1')


#-- Start the calculations
if (length(x) != length(y)) stop('x and y of different length')
n <- length(x)

q <- matrix(ncol=1, nrow=n*(n-1)/2)
k <- 0
for (i in 1:(n-1)){
  for (j in (i+1):n){
    k <- k+1
    q[k] <- (y[j] - y[i])/(x[j] - x[i])
  }
}

#-- Confidence interval for beta
z <- qnorm(1-alpha/2)
k <- n*(n-1)/4 - z*sqrt(n*(n-1)*(2*n+5) / 72)
low <- q[k+1]
upp <- q[n*(n-1)/2 - k]
icept<-median(y-median(q)*x)
c(icept,median(q),low,upp)
}

#theil(mean1, diff1)

