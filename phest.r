phest<-function(est, cov, pos, label=NULL, alpha=0.05, print.all = FALSE, exp=TRUE){
  # Name: phest
  # Desc: Function corresponding to the SAS estimate statement
  # Auth: sven.sandin@meb.ki.se
  # Date: 12-MAR-2004
  
  # 02JUN04: Bug fix. Replaced c8<-2*(1-pnorm(c2/c4)) with c8<-2*(1-pnorm(abs(c2/c4)))
  # 01DEC04: Updated. Added slask vector since >= R2.0.0 no longer accept NA in subscripts
  # 21FEB05: Updated. Added check and procedure for NA parameter estimates
  # 30SEP15: Updated. Now allow for label parameter to be left empty
  # 30SEP15: Updated. Added parameter exp for exponentiating estimates
  # 30SEP15: Updated. Allow cov to be null. Then vcov(est) is used
  
  # est....A vector. Vector with estimates
  # cov....A Matrix. Covariance matrix
  # pos....A Matrix. Positions and sign for estimate. E.g c(-2,3) is 0 -1 1. The pos matrix must take NA instead of zero as in SAS
  # label..Character vector. Labes describing the different estimates. Length should be the same as the number of rows in the pos matrix. If left empty the letter "A", "B" etc will be used
  # exp....: Boolean. If TRUE then estimates will be anti-logarithms. This will also control the label of the column containing the point estimate "Estimate" or "Risk Ratio"

  # Example
  # -------
  # f1=glm(outcome ~ -1 + asex:bsex, data=t1, family = binomial())
  # 
  # mat <- matrix(NA, ncol=4, nrow=length(lbl))
  # mat[1,]   <- c(1, -2, NA, NA); #Correspond to SAS contrast 1 -1 0 0
  # mat[2,]   <- c(1, -2, 3, -4); #Correspond to SAS contrast 1 -1 1 -1
  # 
  # phest(f1$coef, vcov(f1), mat, print.all=T)$x
  
  
  if (is.null(est) | is.null(pos)){
    stop('Parameters phest(est, pos) must all be entered')
  }
  
  if (is.null(cov)){
    writeLines("Using vcov(est) as covariance matrix")
    cov=vcov(est)
  }

  if (is.null(label)){
    writeLines("label not specified so letters A, B, C etc used to describe the estimates")
    label=LETTERS[1:NROW(pos)]
  }
    
  if (class(cov) != "matrix") stop('Parameter cov must be of type matrix')
  if (NROW(cov) != NCOL(cov)) stop('Covariance matrix must be square')
  if (NROW(est) != NROW(cov)) stop('Parameter vector not the same dimension as covariance matrix')
  if (alpha>0.99 | alpha<0.01) stop('Parameter alpha not between 0.01 and 0.99')
  
  rem.index <- c(1:length(est))[is.na(est)==T]
  
  if ( length(rem.index)>0 ) {
    warning(c("Non-estimable parameter estimates removed"))
    est <- est[-rem.index]
    cov <- cov[-rem.index, -rem.index]
  }
  
  
  c1<-matrix(0,ncol=NROW(est),nrow=NROW(pos))
  for (i in 1:NROW(pos)) {
    # vector slask added for R2.0.1 svesan 01dec20
    slask<-pos[i,]
    slask<-na.omit(slask)
    c1[i,abs(slask)] <- sign(slask)
    rm(slask)
  }
  #c1[i,abs(pos[i,])] <- sign(pos[i,]) svesan 01dec2004
  
  c2<-c1%*%est
  c3<-c1%*%cov%*%t(c1)
  c4<-sqrt(c3[col(c3)==row(c3)])
  c5<-c2-c4*qnorm(1-alpha/2)
  c6<-c2+c4*qnorm(1-alpha/2)
  
  # 2*(1-P(X<est/std))
  c8<-2*(1-pnorm(abs(c2/c4)))
  
  #if (NROW(c8[c8<0])>0 | NROW(c8[c8>1])>0) stop('p-value>1 or <0')
  
  c7<-cbind(round(cbind(exp(c2),c4,exp(c5),exp(c6),c8),digits=4))
  dimnames(c7)<-list(label,c("Risk Ratio","SD","Lower","Upper","P-value"))
  if (exp==FALSE){
    c7<-cbind(round(cbind(c2,c4,c5,c6,c8),digits=4))
    dimnames(c7)<-list(label,c("Estimate","SD","Lower","Upper","P-value"))
  }
  print(c7)

  
  if (print.all)
    list(coefficients=est, estimates=c2, x=c7,
         lower=c5, upper=c6, cov=c3,
         estimates.exp=exp(c2), lower.exp=exp(c5), upper.exp=exp(c6),
         alpha=alpha, contrasts=c1)
}

