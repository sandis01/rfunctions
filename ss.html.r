ss.html <- function(infile, file=NULL, font="courier") {

# Name: ss.html
# Desc: Read R sink output and converte to HTML file
# Auth: sven.sandin@meb.ki.se
# Date: 24-Sep-2004

# filename..: Character string for full image file name
# file......: Output html file
# font......: Font in html file

# 07-oct-2004: Added check that infile exist

#-- Trim blanks
infile <-sub("^ +", "", sub(" +$", "", infile))

if (is.null(infile)) stop('Parameter infile must be entered')
if (file.exists(infile)==FALSE) stop(paste('File',infile,"do not exist"))

if (is.null(file) & substr(infile,nchar(infile)-3,nchar(infile)-3)==".") {
  file <- gsub(" ","",paste(substr(infile,1,nchar(infile)-4), ".html", split=""))
}

if (is.null(file)) stop('Parameter file must be entered')


#-- Read the ascii file into a data frame
a<-scan(infile, what=character(150), sep="!", multi.line=T, blank.lines.skip=F, quote=NULL)

#"z:/R/wlh0401/result_24sep04.txt"

#-- If text do not start with <p> then add <BR> at the eol
sub<-substr(a,1,4)

for (i in 1:length(a)) {
  if (sub[i]!="<img") a[i]<-paste(a[i], "<br>", split="")
}

#-- Replace blanks with html blank tag
a<-gsub("                    ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("                   ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("                  ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("                 ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("                ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("               ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("              ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("             ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("            ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("           ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("          ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("         ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("        ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("       ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("      ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("     ", "&nbsp;&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("    ", "&nbsp;&nbsp;&nbsp; ", a)
a<-gsub("   ", "&nbsp;&nbsp; ", a)
a<-gsub("  ", "&nbsp; ", a)


#-- Document header
pre<-c('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">','<html>','<head>','  <meta content="text/html; charset=ISO-8859-1"',' http-equiv="content-type">','  <title>sf</title>','</head>','<body>')

#-- End of file text to html file
post<-c('<br>','</body>','</html>')

#-- Print header to html file
write(pre, file=file)

#-- Print updated text output to html file
write(a, file=file, append=T)

#-- Print end of file text to html file
write(post, file=file, append=T)

}
#ss.html(infile="z:/R/wlh0401/result_24sep04.txt")

#ss.html(infile="~/R/wlh0401/gnu.txt", file="~/R/wlh0401/gnu.html")
