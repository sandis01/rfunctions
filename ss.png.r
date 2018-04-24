"ss.png" <- function(filename=NULL, h=11, w=14, unit="cm", getwd=TRUE, ...) {

# Name: ss.png
# Desc: Print HTML image tag to sink file when calling png statement
# Auth: sven.sandin@meb.ki.se
# Date: 24-Sep-2004

# filename..: Character string for full image file name
# unit......: Measurement unit for the height and width parameters. cm or px
# h.........: Image height in HTML tag
# w.........: Image width in HTML tag

# 07-Dec-2005: Added the parameter and the functionality of getwd

if (is.null(filename)) stop('Parameter filename must be entered')

#-- Next row added 07-Dec-2005
if (date()<as.Date('07dec2005','%d%b%Y') & getwd==TRUE) filename=paste(getwd(),"/",filename,sep="")

if (unit!="cm" & unit!="px") stop('Parameter unit must be "cm" or "px"')

a<-'<img alt="ss.png-file" src="file:///'
b<-'" style="width:'
c<-'; height:'
d<-';"><br>'

c<-paste(a, filename, b, w, unit, c, h, unit, d, split="")
c<-gsub("file:/// ", "file:///", c)
c<-gsub(' " style="', '" style="', c)
c<-gsub(" cm", "cm", c)
c<-gsub(" px", "px", c)
c<-gsub(" ;", ";", c)

#-- Create the PNG file ;
png(filename=filename, ...)

#-- Write include graph tag to the sink-file;
cat(c)

}
#ss.png(filename="asf", height=10, width=13)
