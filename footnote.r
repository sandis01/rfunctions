footnote<-function(study=NULL, pgm=NULL, text=FALSE, line=3, adj=0, ...){
  # Name: footnote
  # Desc: Generate a footnote for graphs (default) or text output (when text=TRUE)
  # Auth: sven.sandin@meb.ki.se
  # Date: 16-AUG-2004
  # The adj=0 left align the footnote while adj=0.5 or 1 would center or right align the text
  # 27-sep-2004: Changed search path for user name
  # 18-jan-2005: Added the adj=0 parameter to the function. Earlier in the mtext call
  # 24-apr-2018: Added the text=F parameter to the function to allow time tag for text output as well

  # study.......: Study id
  # pgm.........: Program name
  # example 1...: footnote(study="wlh0301", pgm="dsfsd.r")
  # example 2...: footnote(pgm="dsfsd.r")
  # example 3...: footnote()
  # example 4...: footnote(pgm="dsfsd.r", text=TRUE)

  us<-Sys.getenv("username")
  if(us=="") us<-Sys.getenv("USERNAME")
  if(us=="") us<-Sys.getenv("user")
  if(us=="") us<-Sys.getenv("USER")

  if (is.null(pgm)==FALSE & is.null(study)==FALSE) tmp<-paste(study,"/", us,"/pgm=",pgm,"/",substr(date(),9,10),substr(date(),5,7),substr(date(),20,24),"/R",R.Version()$major,".",R.Version()$minor,"/",Sys.info()[1],Sys.info()[2],sep="")
  else if (is.null(pgm)==FALSE) tmp<-paste(us, "/pgm=", pgm, "/", substr(date(),9,10), substr(date(),5,7), substr(date(),20,24), "/R", R.Version()$major, ".", R.Version()$minor, "/", Sys.info()[1], Sys.info()[2], sep="")
  else if (is.null(study)==FALSE) tmp<-paste(study,"/",us,"/pgm=",pgm,"/",substr(date(),9,10),substr(date(),5,7),substr(date(),20,24),"/R",R.Version()$major,".",R.Version()$minor,"/",Sys.info()[1],Sys.info()[2],sep="")
  else tmp<-paste(us,"/",substr(date(),9,10),substr(date(),5,7),substr(date(),20,24),"/R",R.Version()$major,".",R.Version()$minor,"/",Sys.info()[1],Sys.info()[2],sep="")

  #-- 24APRIL2018 by svesan@ki.se
  if (text==TRUE)  print(tmp)
  else mtext(tmp, side=1, line=line, adj=adj, ...)
}

