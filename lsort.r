"lsort"<-
function(data, by = 1:dim(data)[2], asc = rep(T, length(by)), na.last = T)
{
# 
# this function works like the SAS SORT procedure
#
# data...: a dataframe or a matrix, "dimnames" are used as the variable names
#
# by.....: a vector or a list of variable names or column indices specifying 
#          the "sort by" variables, the default is to sort by all variables
#          in the order they appear in the data set
#
# asc....: a vector with the same length as "by" indicating whether the sorting
#          of each "by" variable is in ascending order, the default order is
#          ascending
#
# na.last: a flag indicating whether missing values are placed as the last 
#          elements in the data set, the default is T
#
# example:
#
#	1.  sort "mydata" by "id" (ascending) and "group" (descending)
#
#		newdata <- lsort(mydata, by=c("id","group"), asc=c(T,F))
#
#	2.  sort "mydata" by the third and the first variable (ascending)
#
#		newdata <- lsort(mydata, by=c(3,1))
#
#	3.  sort "mydata" by "id" and the 7th variable (ascending)
#
#		newdata <- lsort(mydata, by=list("id",7))
# 
	m <- dim(data)[1]
	keys <- 1:m
	rotate <- m:1
	for(i in length(by):1) {
		if(asc[i])
			keys[] <- keys[sort.list(data[, by[[i]]][keys], na.last
				 = na.last)]
		else keys[] <- keys[order(data[, by[[i]]][keys], rotate, 
				na.last = na.last)[rotate]]
	}
	data[keys,  ]
}

