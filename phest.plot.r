phest.plot <- function(xx, labels="", vref=NULL, lim=0.05, xlim=NULL, log=FALSE, lty=1, bcol=NULL, xlow=NULL, xupp=NULL, cex=1, pch=NULL, point.est=FALSE, ci=FALSE, maxdec=2){

  #--------------------------------------------------------------------------
  # Name   : phest.plot.r
  # Desc...: Plotting of confidence intervals and point estimates using a
  #          matrix created by the phest.r program
  # Date...: 18-May-2004
  # Author.: Sven Sandin, Karolinska Institutet, sven.sandin@meb.ki.se
  #
  # Parameters:
  # -----------
  # xx.......: An matrix with point estimate, lower and upper CI limits as
  #            column 1, 3 and 4. Typically this is a matrix created by the
  #            function phest()$xx
  # labels...: A vector of the same length as the XX matrix rows. Labeles used
  #            to label the confidence intervals
  # lim......: Confidence interval width. Default 0.05
  # xlim.....: x-axis min and max limits. (Only applicable for log=FALSE)
  # log......: TRUE or FALSE. When TRUE the XX data are drawn on a log2-scale.
  #            E.g. 1/4, 1/2, 1, 2, 4.
  # xupp.....: Upper x-axis limit.
  # xlow.....: Lower x-axis limit. If log=T then this is the denominator on
  #            the scale 1/2, 1/4, 1/8 etc
  # vref.....: Vector of vertical reference lines
  # lty......: Vector of the same length as VREF giving line types for VREF
  # bcol.....: Vector of colors to fill the confidence interval boxes. The
  #            colors are mapping the CIs from the bottom to the top
  # pch......: Integer 17-23 for plotting point estimates
  # cex......: Change font size
  # point.est: Print point estimate numbers on graph
  # ci.......: Print confidence interval numbers on graph
  # maxdec...: Decimals used when printing ci and point.est
  #
  # Update 02dec2004 sven sandin
  # 1. Added the point.est, ci and maxdec parameters
  #--------------------------------------------------------------------------

  if (is.null(labels)==TRUE & is.null(dimnames(xx)[[1]])==FALSE) labels <- dimnames(xx)[[1]]
  else if (is.null(labels)==TRUE & is.null(dimnames(xx)[[1]])==FALSE) labels <- as.character(1:NROW(xx))
  id<-factor(1:NROW(xx), levels=1:NROW(xx), label=labels)
  id.num<-as.numeric(id)

if (is.null(xlow)==FALSE) if (xlow-round(xlow,0)!=0) stop("xlow must be integer")
if (is.null(xupp)==FALSE) if (xupp-round(xupp,0)!=0) stop("xupp must be integer")
if (is.null(xlow)==FALSE & is.null(xupp)==FALSE) if (xupp<xlow) stop("xupp < xlow")

if (is.null(pch)==FALSE) {
  if ((pch>=17 & pch<=23)==FALSE) stop("pch must be integer between 17 and 23")
}

if (log==TRUE) {
  zz<-log2(xx)
  a1 <- -10:10
  a2 <-   c("1/1024","1/512","1/256","1/128","1/64","1/32","1/16","1/8","1/4","1/2","1","2","4","8","16","32","64","128","256","512","1024")

  if (is.null(xlow)==TRUE) a.l<-floor(min(zz[,3]))
  else a.l <- log2(1/xlow)

  if (is.null(xupp)==TRUE) a.u<-ceiling(max(zz[,4]))
  else a.u <- log2(xupp)

  a3<-a1[a1>=a.l & a1<=a.u]
  a4<-a2[a1>=a.l & a1<=a.u]
  plot(zz[,1], id, ylab="",xlab="", xlim=c(a.l,a.u),
  ylim=c(min(id.num), max(id.num)+1), axes=F)
  #ylim=c(min(id.num)*0.9,max(id.num)*1.1), axes=F)
  rect(zz[,3],as.numeric(id)-lim,zz[,4],as.numeric(id)+lim,border="black",col=bcol)
  #axis(1)
  if (is.null(pch)==FALSE) points(zz[,1], id, pch=pch)
  axis(1, at=a3, labels=a4)
  text(zz[,1], id.num+0.05, levels(id), pos=3, cex=cex)
  if (is.null(vref)==FALSE) abline(v=log2(vref), lty=lty)

  #-- Add point estimates and confidence intervals numbers
  if (point.est==T) text(zz[,1], id.num-0.05, as.character(round(xx[,1],maxdec)), pos=1, cex=cex)

  if (ci==T) {
    text(zz[,3], id.num-0.05, as.character(round(xx[,3],maxdec)), pos=1, cex=cex)
    text(zz[,4], id.num-0.05, as.character(round(xx[,4],maxdec)), pos=1, cex=cex)
  }
}
else {
  xlm <- c(min(xx[,3]),max(xx[,4]))
  if (is.null(xlim)==FALSE) xlm <- xlim

  plot(xx[,1], id, xlim=xlm, ylab="",xlab="",
  ylim=c(min(id.num)*0.9,max(id.num)*1.1), axes=T)
  #axis(2,at=id,labels=levels(id))
  rect(xx[,3],as.numeric(id)-lim,xx[,4],as.numeric(id)+lim,border="black")
  axis(1)
  
  #-- Add the bar labels
  text(xx[,1], id.num+0.05, levels(id), pos=3, cex=cex)
  if (is.null(vref)==FALSE) abline(v=vref)
  
  #-- Add point estimates and confidence intervals numbers
  if (point.est==T) text(xx[,1], id.num-0.05, as.character(round(xx[,1],maxdec)), pos=1, cex=cex)

  if (ci==T) {
    text(xx[,3], id.num-0.05, as.character(round(xx[,3],maxdec)), pos=1, cex=cex)
    text(xx[,4], id.num-0.05, as.character(round(xx[,4],maxdec)), pos=1, cex=cex)
  }
}
}