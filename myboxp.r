myboxp <- function(var, bystmt, vzero=TRUE, low.lim=0.1, upp.lim=0.9, points=FALSE, ...) {

  # Name: myboxp
  # Desc: Box plot with whiskers between percentiles of the data instead of the Tukey 1.5*QRANGE
  #       Default the individual data points are not drawn. Include this by (..., outline=TRUE)
  # Auth: sven.sandin@meb.ki.se
  # Date: 12-MAR-2004
  #
  # var....: Y-axis response variable
  # bystmt.: Factor. One box for each factor level
  # vzero..: Default y-axis start at zero. vzero=F changes this
  # low.lim: The lower percentile. Default 10%
  # upp.lim: The upper percentile. Default 90%
  # points.: Include data points. Default no. 
  #
  # 02JUN05: First version. Developed for Win_XP, R 2.0.1

  hepp<-function (x) {
    as.matrix(c(as.vector(quantile(x, probs=c(low.lim))), as.vector(quantile(x, probs=c(upp.lim)))))
  }

  a<-boxplot(var ~ bystmt, plot=FALSE)
  g<-unlist(tapply(var, bystmt, FUN=hepp))


  a$stats[c(1,5),]<-matrix(g, nrow=2)

  if (vzero==TRUE) {
    bxp(a, outline=FALSE, ylim=c(0,ceiling(max(g))), ...)
  }
  if (vzero==FALSE) {
    bxp(a, outline=FALSE, ylim=c(min(g), ceiling(max(g))), ...)
  }

}

#myboxp(egg, diet.score)
