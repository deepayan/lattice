
## We want data frame methods that support calls like xyplot(data,
## formula, ...) by calling them as xyplot(formula, data, ...). This
## is a little tricky (in ways that I don't really understand) because
## of the need to handle NSE arguments like groups and subset.

## Here we _know_ that the first argument (or a named argument 'x') is
## a data.frame. We will try to identify the 'x' and 'formula'
## arguments, which may or may not be named, remove them (along with a
## 'data' argument if any), rename them to 'data' and 'x'
## respectively, and evaluate the call.


.df2formula <- function(ccall)
{
    ocall <- ccall
    n <- length(ccall)
    if (n == 2) stop("a formula must be specified for the 'data.frame' method")
    cnames <- names(ccall)
    if (is.null(cnames)) {
        ## No named arguments --- this is relatively easy
        if (n == 3) names(ccall) <- c("", "data", "x")
        else if (n > 3) {
            ## third argument is 'formula', and second argument 'data'
            ## is to be ignored
            ccall <- ccall[-3L] # second argument has index 3
            names(ccall) <- rep("", n-1) # second argument is now the formula
            names(ccall)[c(2, 3)] <- c("data", "x")
        }
    }
    else {
        ## OK, we have names (so at least one named argument). Let's
        ## check how many unnamed arguments we have (excluding the
        ## function name), how many of 'x', 'data', and 'formula' are
        ## still missing, and match them accordingly
        w <- which(cnames[-1] == ""); nw <- length(w) 
        have_args <- c("x", "data", "formula") %in% cnames
        need_match <- c("x", "data", "formula")[!have_args]
        nmatch <- min(nw, length(need_match))
        if (nmatch > 0) { # match with missing args
            names(ccall)[ w[1:nmatch] + 1L ] <- need_match[1:nmatch]
        }
        ## We should be ready now to switch argument names and
        ## call. We assume that we have at least 'x' and either or
        ## both of 'data' and 'formula'
        formula <- if (is.null(ccall$formula)) ccall$data
                   else ccall$formula
        ## The next line is highly questionable, but is needed to
        ## handle cases like
        ## xyplot(data, data = y ~ x, , col = 1)
        ## although it's not clear whether they need to be
        if (missing(formula)) formula <- ccall$data
        x <- ccall$x
        ccall$formula <- NULL # in case present
        ccall$x <- formula # switch
        ccall$data <- x
    }
    if (!inherits(eval(ccall$x), "formula")) stop("'formula' must be a formula object")
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}


dotplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(dotplot)
    .df2formula(ocall)
}

barchart.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(barchart)
    .df2formula(ocall)
}

stripplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(stripplot)
    .df2formula(ocall)
}

bwplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(bwplot)
    .df2formula(ocall)
}

wireframe.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(wireframe)
    .df2formula(ocall)
}

cloud.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(cloud)
    .df2formula(ocall)
}

densityplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(densityplot)
    .df2formula(ocall)
}

histogram.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(histogram)
    .df2formula(ocall)
}

contourplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(contourplot)
    .df2formula(ocall)
}

levelplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(levelplot)
    .df2formula(ocall)
}

qq.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(qq)
    .df2formula(ocall)
}

qqmath.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(qqmath)
    .df2formula(ocall)
}

tmd.data.frame <- function(object, formula, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(tmd)
    if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
    modifyList(tmd(formula, object, ...), list(call = ocall))
}

xyplot.data.frame <- function(x, data = NULL, formula = data, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(xyplot)
    .df2formula(ocall)
}

## barchart.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(barchart)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(barchart(formula, x, ...), list(call = ocall))
## }

## stripplot.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(stripplot)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(stripplot(formula, x, ...), list(call = ocall))
## }

## bwplot.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(bwplot)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(bwplot(formula, x, ...), list(call = ocall))
## }

## wireframe.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(wireframe)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(wireframe(formula, x, ...), list(call = ocall))
## }

## cloud.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(cloud)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(cloud(formula, x, ...), list(call = ocall))
## }

## densityplot.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(densityplot)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(densityplot(formula, x, ...), list(call = ocall))
## }

## histogram.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(histogram)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(histogram(formula, x, ...), list(call = ocall))
## }

## contourplot.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(contourplot)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(contourplot(formula, x, ...), list(call = ocall))
## }

## levelplot.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(levelplot)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(levelplot(formula, x, ...), list(call = ocall))
## }

## qq.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(qq)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(qq(formula, x, ...), list(call = ocall))
## }

## qqmath.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(qqmath)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(qqmath(formula, x, ...), list(call = ocall))
## }

## tmd.data.frame <- function(object, formula, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(tmd)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(tmd(formula, object, ...), list(call = ocall))
## }

## xyplot.data.frame <- function(x, data = NULL, formula = data, ...)
## {
##     ocall <- sys.call(); ocall[[1]] <- quote(xyplot)
##     if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
##     modifyList(xyplot(formula, x, ...), list(call = ocall))
## }




