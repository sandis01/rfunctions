ordplt <- function(bar, area, ylab="Cumulative proportion", legend = rownames(tmp),
                   density = 20, gray=TRUE, 
                   col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk"),  ...) {


# Author...: sven.sandin@meb.ki.se
# Date.....: 15-Dec-2004
#
# bar......: A factor. One bar is drawn for each factor level
# area.....: A factor. The segments within bars
#
# 20jan2005: Moved parameters col, legend and density to the function instead of hardcoded
# 02jun2005: 1) Removed print(tmp)
#            2) Added parameter gray=TRUE parameter

     
tmp0 <- as.matrix(table(area, bar))
tmp  <- tmp0

for (i in 1:NROW(tmp) ) {
for (j in 1:NCOL(tmp) ) tmp[i,j] <- tmp[i,j]/sum(tmp0[,j])
}


if (gray==FALSE) {
  barplot(tmp, angle = 15+10*1:5, density = density, col = col, legend = legend, ylab=ylab, ...)
}
else {
  barplot(tmp, legend=legend, ylab=ylab, ...)
}

#title(main = list("Death Rates in Virginia", font = 4))
}

#ordplt(mort$physact, mort$cbmi, xlab="Physical Activity Score")
