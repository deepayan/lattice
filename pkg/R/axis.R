### Copyright 2001-2003  Deepayan Sarkar <deepayan@stat.wisc.edu>
###
### This file is part of the lattice library for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA




calculateAxisComponents <- function(x, ..., abbreviate = NULL, minlength = 4)

    ## This aims to be a general function which given a general
    ## 'range' x and optional at, generates the locations of tick
    ## marks and corresponding labels. 

    ## x is guaranteed to be given (possibly NA). Possible cases
    ## correspond to factors (character/expression), shingle (see
    ## below), "POSIXt", "date" and usual numeric. The last case will
    ## be default, and will be changed later if necessary.

    ## Theres no need to handle shingles specially. Shingles can also
    ## be thought of as numeric, and thus the default is more
    ## appropriate for functions like xyplot. In functions like
    ## bwplot, things will be adjusted elsewhere when one of the
    ## variables is a shingle.

    ## Note that at and labels will never be TRUE (it's set up that
    ## way), so it's enough to check if they are is.logical(), which
    ## means they are not explicitly specified.

    ## The variables about log scales are required for cases where at
    ## is explicitly specified. In such cases, at will be
    ## log(at,base=logbase), but labels would correspond to at.

{
    if (all(is.na(x)))
        return(list(at = numeric(0),
                    labels = numeric(0),
                    check.overlap = TRUE,
                    num.limit = c(0,1)))

    ## Get ticks and labels depending on x (generic defined below)
    ans <- formattedTicksAndLabels(x, ...)

    ## remove labels outside limits

    rng <- range(ans$num.limit)
    ok <- ans$at >= rng[1] & ans$at <= rng[2]
    ans$at <- ans$at[ok]
    ans$labels <- ans$labels[ok]

    ## abbreviate labels if requested

    if (is.logical(abbreviate) && abbreviate)
        ans$labels <- abbreviate(ans$labels, minlength)

    ans
}



formattedTicksAndLabels <- function(x, ...)

    UseMethod("formattedTicksAndLabels")




formattedTicksAndLabels.default <-
    function (x, at = FALSE, labels = FALSE, logsc = FALSE,
              abbreviate = NULL, minlength = 4, format.posixt, ...)

    ## meant for when x is numeric

{
    ## handle log scale (most other methods ignore logsc)

    if (is.logical(logsc) && logsc) logsc <- 10
    have.log <- !is.logical(logsc) || logsc

    logbase <-
        if (is.numeric(logsc)) logsc
        else exp(1)
    logpaste <-
        if (have.log) paste(as.character(logsc), "^", sep = "")
        else ""


    ## will check for overlap only when neither at nor labels is specified

    check.overlap <-
        if (is.logical(at) && is.logical(labels)) TRUE
        else FALSE
        
    if (is.logical(at)) ## at not explicitly specified
    {
        at <- pretty(x[is.finite(x)], ...)
    }
    else if (have.log)  ## and at specified
    {
        if (is.logical(labels)) labels <- as.character(at)
        at <- log(at, base = logbase)
    }
    list(at = at, labels = if (is.logical(labels))
         paste(logpaste, format(at, trim = TRUE), sep = "") else labels,
         check.overlap = check.overlap,
         num.limit = if (length(x) == 2) x else range(x))
}









formattedTicksAndLabels.date <-
    function (x, at = FALSE, labels = FALSE, logsc = FALSE,
              abbreviate = NULL, minlength = 4, format.posixt, ...)
{
    ## handle log scales (not very meaningful, though)

    if (is.logical(logsc) && logsc) logsc <- 10
    have.log <- !is.logical(logsc) || logsc

    logbase <-
        if (is.numeric(logsc)) logsc
        else exp(1)


    ## will check for overlap only when neither at nor labels is specified

    check.overlap <-
        if (is.logical(at) && is.logical(labels)) TRUE
        else FALSE
        
    if (is.logical(at)) ## at not explicitly specified
    {
        at <- as.integer(pretty(x[is.finite(x)], ...))
        class(at) <- "date"
        datelabels <- as.character(at)
    }
    else if (have.log) ## and at specified
    {
        if (is.logical(labels)) labels <- as.character(at)
        at <- log(at, base = logbase)
    }
    list(at = at,
         labels = if (is.logical(labels)) datelabels else labels,
         check.overlap = check.overlap,
         num.limit = if (length(x) == 2) as.numeric(x) else as.numeric(range(x)))
}



## The next two are actually identical


formattedTicksAndLabels.character <-
    function (x, at = FALSE, labels = FALSE, logsc = FALSE,
              abbreviate = NULL, minlength = 4, format.posixt, ...)
{
    ans <- list(at = if (is.logical(at)) seq(along = x) else at,
                labels = if (is.logical(labels)) x else labels,
                check.overlap = FALSE)
    ans$num.limit <- c(0, length(ans$at) + 1)
    ans
}






formattedTicksAndLabels.expression <-
    function (x, at = FALSE, labels = FALSE, logsc = FALSE,
              abbreviate = NULL, minlength = 4, format.posixt, ...)
{
    ans <- list(at = if (is.logical(at)) seq(along = x) else at,
                labels = if (is.logical(labels)) x else labels,
                check.overlap = FALSE)
    ans$num.limit <- c(0, length(ans$at) + 1)
    ans
}





formattedTicksAndLabels.POSIXct <-
    function (x, at = FALSE, labels = FALSE, logsc = FALSE, 
              abbreviate = NULL, minlength = 4,
              format.posixt = NULL, ...) 
{
    ## modified from axis.POSIXct. 

    num.lim <- if (length(x) == 2) as.numeric(x) else as.numeric(range(x))
    mat <- is.logical(at)
    mlab <- is.logical(labels)

    if (!mat)
        x <- as.POSIXct(at)
    else x <- as.POSIXct(x)
    range <- as.numeric(range(x))
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    if (d < 1.1 * 60) {
        sc <- 1
        if (is.null(format.posixt)) 
            format.posixt <- "%S"
    }
    else if (d < 1.1 * 60 * 60) {
        sc <- 60
        if (is.null(format.posixt)) 
            format.posixt <- "%M:%S"
    }
    else if (d < 1.1 * 60 * 60 * 24) {
        sc <- 60 * 24
        if (is.null(format.posixt)) 
            format.posixt <- "%H:%M"
    }
    else if (d < 2 * 60 * 60 * 24) {
        sc <- 60 * 24
        if (is.null(format.posixt)) 
            format.posixt <- "%a %H:%M"
    }
    else if (d < 7 * 60 * 60 * 24) {
        sc <- 60 * 60 * 24
        if (is.null(format.posixt)) 
            format.posixt <- "%a"
    }
    else {
        sc <- 60 * 60 * 24
    }
    if (d < 60 * 60 * 24 * 50) {
        zz <- pretty(z/sc, ...)
        z <- zz * sc
        class(z) <- c("POSIXt", "POSIXct")
        if (is.null(format.posixt)) 
            format.posixt <- "%b %d"
    }
    else if (d < 1.1 * 60 * 60 * 24 * 365) {
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon, ...)
        m <- length(zz$mon)
        m <- rep(zz$year[1], m)
        zz$year <- c(m, m + 1)
        z <- as.POSIXct(zz)
        if (is.null(format.posixt)) 
            format.posixt <- "%b"
    }
    else {
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year, ...)
        z <- as.POSIXct(zz)
        if (is.null(format.posixt)) 
            format.posixt <- "%Y"
    }
    if (!mat) 
        z <- x[is.finite(x)]
    z <- z[z >= range[1] & z <= range[2]]
    labels <- format(z, format = format.posixt)
    list(at = as.numeric(z), labels = labels,
         check.overlap = FALSE,
         num.limit = num.lim)
}


