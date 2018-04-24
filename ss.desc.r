ss.desc <- function(x, by=NULL, desc="n nmiss pctmiss mean median p1 p5 p10 q1 q3 min max p90 p95 p99 var std se", data=NULL, digits=3, title=T) {

#--------------------------------------------------------------------------
# Author...: sven.sandin@meb.ki.se
# Date.....: 22-April-2005
# Purpose..: Summary statistics by levels of a variable (ONE variable only)
# .........: Function only print output. No objects delivered.
# System...: Developed running Win_XP, SP1, R2.0.1
# Example..: ss.desc(c("age","height"), by=smoker, data=ana1)
#
# Summary of parameters
# ---------------------
# x......: A vector of variable names in the DATA data frame, e.g. c("a","b)
# by.....: By-variable variable name. Should be a factor
# desc...: Character string of summary measures
# data...: A data frame
# digits.: Number of decimals for the output
# titie..: True or False. Print title or not.
#
#--------------------------------------------------------------------------
# Date.....: 22-April-2005, sven.sandin@ki.se
# 1) Added summary measures std (or se) and var as valid measures
#--------------------------------------------------------------------------

sumnas<-function(w) {sum(is.na(w)==T)}
sumn  <-function(w) {sum(is.na(w)==F)}
ammis <-function(w) {100*sum(is.na(w)==T)/length(w)}

attach(data)

if (class(by) != "factor") stop('Parameter by must be a factor')
if (class(data) != "data.frame") stop('Parameter data must be a data frame')
if (class(title) != "logical") stop('Parameter title must be TRUE or FALSE')

if (is.null(by)==FALSE) antby <- length(unique(by))

n <- length(x)
m <- n*antby

res <- matrix("", ncol=18, nrow=m)
var.lbl<-c("")
a<-1; b<-antby
des.lbl <- c("NA")

#vb  <- as.vector(data[names(data) %in% c(deparse(substitute(by)))])
#vb  <- as.vector(vb[,1])
#attach(data)
vb<-by
detach(data)

for (i in 1:n) {
  var <- data[names(data) %in% c(x[i],deparse(substitute(by)))]
  vx  <- as.vector(data[names(data) %in% c(x[i])])
  vx  <- as.vector(vx[,1])

  mat <- matrix(NA, ncol=16, nrow=antby)

  j <- 0
  if (length(grep("n", desc)>0)==T) {
    j <- j+1
    mat[,j]<-t(tapply(vx, vb, FUN=sumn))
    if (i==1) des.lbl<-c(des.lbl,"N")
  }
  if (length(grep("nmiss", desc)>0)==T) {
    j <- j+1
    mat[,j]<-t(tapply(vx, vb, FUN=sumnas))
    if (i==1) des.lbl<-c(des.lbl,"NMISS")
  }
  if (length(grep("pctmiss", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=ammis)), digits=0)
    if (i==1) des.lbl<-c(des.lbl,"%Missing")
  }
  if (length(grep("min", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=min, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"Min")
  }
  if (length(grep("p1", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.01, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"1% Pctl")
  }
  if (length(grep("p5", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.05, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"5% Pctl")
  }
  if (length(grep("p10", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.1, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"10% Pctl")
  }
  if (length(grep("q1", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.25, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"Q3")
  }
  if (length(grep("mean", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=mean, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"Mean")
  }
  if (length(grep("median", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=median, na.rm=T)), digits=digits)
        if (i==1) des.lbl<-c(des.lbl,"Median")
  }
  if (length(grep("q3", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.75, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"Q3")
  }
  if (length(grep("p90", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.9, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"90% Pctl")
  }
  if (length(grep("p95", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.95, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"95% Pctl")
  }
  if (length(grep("p99", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=quantile, probs=0.99, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"99% Pctl")
  }
  if (length(grep("max", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=max, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"Max")
  }
  if (length(grep("var", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(t(tapply(vx, vb, FUN=var, na.rm=T)), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"Var")
  }
  if (length(grep("std", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(sqrt(t(tapply(vx, vb, FUN=var, na.rm=T))), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"STD")
  }
  if (length(grep("se", desc)>0)==T) {
    j <- j+1
    mat[,j]<-round(sqrt(t(tapply(vx, vb, FUN=var, na.rm=T))), digits=digits)
    if (i==1) des.lbl<-c(des.lbl,"se")
  }

  res[a:b, 1:j] <- mat[,1:j]
  a<-a + antby; b<-b + antby
  var.lbl <- c(var.lbl, rep(x[i], antby))
}

var.lbl<-var.lbl[-1]
by.lbl  <- rep(levels(by), n)

#mat.lbl <- c("Variable", "Subgroup", des.lbl[-1])
#final<-cbind(var.lbl, by.lbl, res)[,1:length(mat.lbl)]

mat.lbl <- c("Subgroup", des.lbl[-1])
final<-cbind(by.lbl, res)[,1:length(mat.lbl)]
rownames(final) <- var.lbl

colnames(final) <- mat.lbl

if (title==T) writeLines(paste("Summary statistics by the variable: ", deparse(substitute(by)), collapse=""))

print(noquote(final))

#rm(sumnas, sumn, ammis, n, m, res, var.lbl, a, b, des.lbl, i, vx, vb, mat, j, by.lbl, final)
#gc(TRUE)

}
#ss.desc(c("age", "height", "bmi"), by=smoker, data=wlh0404)

#ss.desc(c("packyrs"), by=smoker, data=ana1)
