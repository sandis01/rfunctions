gamterms <- function(fit, se.fit = T)
{
# reconstruct the data, without transformations
	Terms <- terms.inner(as.formula(fit$formula))
	keep <- match(c("", "formula", "data", "subset", "na.action"), names(
		fit$call), nomatch = 0)
	m <- fit$call[keep]
	m$formula <- Terms
	m[[1]] <- as.name("model.frame")
	data <- eval(m, sys.parent())	# Now get the terms, and match them	
	fit$na.action <- null
	termp <- predict(fit, type = "terms", se.fit = se.fit)
	vnames <- attr(Terms, "term.labels")
	tname1 <- attr(terms(as.formula(fit$formula)), "term.labels")
	if(se.fit)
		tfit <- termp$fit
	else tfit <- termp
	if(is.matrix(tfit))
		tname2 <- dimnames(tfit)[[2]]
	else {
		tfit <- as.matrix(tfit)
		tname2 <- names(fit$assign)
		tname2 <- tname2[tname2 != "(Intercept)"]
	}
	if(nrow(data) > nrow(tfit)) {
		keep <- match(row.names(data), dimnames(tfit)[[1]])
		data <- data[!is.na(keep),  , drop = F]
	}
	nterm <- length(tname2)
	outlist <- list(constant = attr(termp$fit, "constant"))
	for(i in 1:nterm) {
		k <- match(tname2[i], tname1)
		xx1 <- data[[vnames[k]]]
		xx2 <- sort(unique(xx1))
		keep <- match(xx2, xx1)
		if(se.fit) {
			if(is.matrix(termp$se.fit))
				zz <- data.frame(x = xx2, y = tfit[keep, i], se
				   = termp$se.fit[keep, i])
			else zz <- data.frame(x = xx2, y = tfit[keep, i], se = 
				  termp$se.fit[keep])
		}
		else {
			zz <- data.frame(x = xx2, y = tfit[keep, i])
		}
		outlist[[vnames[k]]] <- zz
	}
	outlist
}

plotterm <- function(fit, term=1, se=T, p=.95, rug.=T, const, ...) {
    temp <- gamterms(fit, se=se)
    xlab <- (names(temp))[term+1]
    tmat <- temp[[term+1]]
    if (missing(const)) const <- 0
    else if (!is.numeric(const)) {
	if (const==T) const <- temp[[1]]
	else const <- 0
	}

    if (se) {
	ci <- -1*qnorm((1-p)/2)
	ymat <- cbind(tmat[,2], tmat[,2] - ci*tmat[,3], tmat[,2] + ci*tmat[,3])
		      + const
	matplot(tmat[,1], ymat, type='l', ..., xlab=xlab, lty=c(1,2,2), col=1)
	if (rug.) rug(tmat[,1])
	}
    else plot(tmat[,1], tmat[,2]+ const, type='l', ..., xlab=xlab, ylab='')
    }
}

terms.inner <- function(formula)
{
	if(inherits(formula, "formula")) {
		if(length(formula) > 2)
			formula[[2]] <- NULL
		maxch <- 50
		repeat {
			z <- .C("all_names",
				list(formula),
				as.integer(F),
				labels = character(maxch),
				n = as.integer(maxch),
				expr = as.expression(character(maxch)),
				as.logical(T),
				NAOK = T)
			if(z$n >= 0)
				break
			maxch <- 2 * maxch
		}
		xseq <- seq(length = z$n)
		Terms <- z$expr[xseq]
	}
	else Terms <- formula
	if(!inherits(Terms, "terms")) {
		if(data.class(Terms) == "expression")
			terms(paste("~", paste(as.character(Terms), collapse
				 = "+")))
		else stop("invalid object provided as formula or terms expression"
				)
	}
	else Recall(attr(Terms, "formula"))
}
 
