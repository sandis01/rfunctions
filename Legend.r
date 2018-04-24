"Legend" <- function (x="right", y="top", ..., offset=0.25)
{
# Name: Legend
# Desc: fixes a legend to one of the corners of a plot
# Auth: Alexander.Ploner@meb.ki.se
#
# TODO:
#
# Chng:

    x <- match.arg(x,c("left","center","right"))
    y <- match.arg(y,c("bottom","center","top"))
    cc <- par("usr")
    if (x=="left") {
        x  <- cc[1] + xinch(offset)
        xjust <- 0
    } else if (x=="center") {
        x  <- (cc[1] + cc[2])/2
        xjust <- 0.5
    } else if (x=="right") {
        x  <- cc[2] - xinch(offset)
        xjust <- 1
    }
    if (y=="bottom") {
        y  <- cc[3] + yinch(offset)
        yjust <- 0
    } else if (y=="center") {
        y  <- (cc[3] + cc[4])/2
        yjust <- 0.5
    } else if (y=="top") {
        y  <- cc[4] - yinch(offset)
        yjust <- 1
    }
    legend(x, y, xjust=xjust, yjust=yjust, ...)

}