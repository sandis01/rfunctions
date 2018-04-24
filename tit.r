tit <- function (title, prefix=">>>", pre.skip=0, post.skip=0, len=40, max.len=115) {


# Author..: sven.sandin@meb.ki.se
# Date....: 31-sep-2004

# title....: Title row
# prefix...: Prefix text to paste before the title text
# pre.skip.: No of empty lines before the title rows
# post.skip: No of empty lines after the title rows
# len......: Minimum length of --- lines before and following the title text

# Updated..: 28JUN2005 added the max.len parameter. Earlier hardcoded to 75

l1 <- min(max(nchar(sub("^ +", "", sub(" +$", "", title))), len), max.len) + nchar(prefix) + 1

l2 <- paste(unlist(strsplit(rep("-",l1),"")), collapse="")

title <- paste(prefix, title)

#-- Output empty lines before title rows
if (pre.skip>0) {for (i in 1:pre.skip) writeLines("")}

writeLines(c(l2, title, l2))

#-- Output empty lines after title rows
if (post.skip>0) {for (i in 1:post.skip) writeLines("")}

}
