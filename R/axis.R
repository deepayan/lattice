### Copyright (C) 2001-2005  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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





## (By popular demand) function to get axis limits of current panel

current.panel.limits <- function(unit = "native")
{
    list(xlim = convertX(unit(c(0,1), "npc"), unit, valueOnly=TRUE),
         ylim = convertY(unit(c(0,1), "npc"), unit, valueOnly=TRUE))
}


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


## quick and dirty fix: have methods for all sorts of objects, even
## ones in other packages (like chron)


formattedTicksAndLabels.default <-
    function (x, at = FALSE,
              used.at = NULL,
              num.limit = NULL,
              labels = FALSE,
              logsc = FALSE,
              abbreviate = NULL,
              minlength = 4,
              format.posixt, ...)
    ## meant for when x is numeric
{
    range <-
        if (length(x) == 2) as.numeric(x)
        else range(as.numeric(x))

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
         num.limit = range)
}










formattedTicksAndLabels.date <-
    function (x, at = FALSE,
              used.at = NULL,
              num.limit = NULL,
              labels = FALSE,
              logsc = FALSE,
              abbreviate = NULL,
              minlength = 4,
              format.posixt, ...)
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
    function (x, at = FALSE,
              used.at = NULL,
              num.limit = NULL,
              labels = FALSE,
              logsc = FALSE,
              abbreviate = NULL,
              minlength = 4,
              format.posixt, ...)
{
    retain <- if (is.null(used.at) || any(is.na(used.at))) TRUE else used.at
    ans <- list(at = if (is.logical(at)) seq(along = x)[retain] else at,
                labels = if (is.logical(labels)) x[retain] else labels,
                check.overlap = FALSE)
    ans$num.limit <- c(-1, 1) * lattice.getOption("axis.padding")$factor + 
        if (is.null(num.limit) || any(is.na(num.limit))) range(ans$at)
        else num.limit
    ans
}






formattedTicksAndLabels.expression <-
    function (x, at = FALSE,
              used.at = NULL,
              num.limit = NULL,
              labels = FALSE,
              logsc = FALSE,
              abbreviate = NULL,
              minlength = 4,
              format.posixt, ...)
{
    retain <- if (is.null(used.at) || any(is.na(used.at))) TRUE else used.at
    ans <- list(at = if (is.logical(at)) seq(along = x)[retain] else at,
                labels = if (is.logical(labels)) x[retain] else labels,
                check.overlap = FALSE)
    ans$num.limit <- c(-1, 1) * lattice.getOption("axis.padding")$factor + 
        if (is.null(num.limit) || any(is.na(num.limit))) range(ans$at)
        else num.limit
    ans
}



## FIXME: add a method for "Date" here (regurgitate axis.Date)

formattedTicksAndLabels.Date <-
    function (x, at = FALSE,
              used.at = NULL,
              num.limit = NULL,
              labels = FALSE,
              logsc = FALSE, 
              abbreviate = NULL,
              minlength = 4,
              format.posixt = NULL, ...) 
{
    num.lim <-
        if (length(x) == 2) as.numeric(x)
        else as.numeric(range(x))
    mat <- is.logical(at)
    if(!mat) x <- as.Date(at) else x <- as.Date(x)
    range <- range(num.lim)
    range[1] <- ceiling(range[1])
    range[2] <- floor(range[2])
    ## find out the scale involved
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    class(z) <- "Date"
    if (d < 7) # days of a week
        if (is.null(format.posixt)) format.posixt <- "%a"
    if (d < 100) { # month and day
        z <- structure(pretty(z), class="Date")
        if (is.null(format.posixt)) format.posixt <- "%b %d"
    } else if (d < 1.1*365) { # months
        zz <- as.POSIXlt(z)
        zz$mday <- 1;
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1], m)
        zz$year <- c(m, m+1)
        z <- .Internal(POSIXlt2Date(zz))
        if(is.null(format.posixt)) format.posixt <- "%b"
    } else { # years
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$mon <- 0
        zz$year <- pretty(zz$year)
        z <- .Internal(POSIXlt2Date(zz))
        if(is.null(format.posixt)) format.posixt <- "%Y"
    }
    if(!mat)
        z <- x[is.finite(x)] # override changes
    z <- z[z >= range[1] & z <= range[2]]
    z <- structure(sort(unique(z)), class = "Date")    
    if (is.logical(labels))
        labels <- format.Date(z, format = format.posixt)
##     if (identical(labels, TRUE))
##         labels <- format.Date(z, format = format.posixt)
##     else if (identical(labels, FALSE))
##         ## suppress labelling of ticks
##         labels <- rep("", length(z))
    list(at = as.numeric(z),
         labels = labels,
         check.overlap = FALSE,
         num.limit = num.lim)
}






formattedTicksAndLabels.POSIXct <-
    function(x, at = FALSE,
             used.at = NULL,
             num.limit = NULL,
             labels = FALSE,
             logsc = FALSE, 
             abbreviate = NULL,
             minlength = 4,
             format.posixt = NULL, ...) 
{
    ## modified from axis.POSIXct. 
    num.lim <- ## could be reversed
        if (length(x) == 2) as.numeric(x)
        else as.numeric(range(x))
    mat <- is.logical(at)
    mlab <- is.logical(labels)
    if (!mat) x <- as.POSIXct(at)
    else x <- as.POSIXct(x)
    range <- range(num.lim)
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
    if (mlab) labels <- format(z, format = format.posixt)
    list(at = as.numeric(z),
         labels = labels,
         check.overlap = FALSE,
         num.limit = num.lim)
}





## chron 'times' objects


formattedTicksAndLabels.times <-
    function (x, at = FALSE, used.at = NULL,
              num.limit = NULL, labels = FALSE, logsc = FALSE, 
              abbreviate = NULL, minlength = 4, simplify = TRUE, 
              ...) 
{
    ## most arguments ignored for now

    check.overlap <-
        if (is.logical(at) && is.logical(labels)) TRUE
        else FALSE

    if (!inherits(x, "times")) 
        x <- chron::chron(x)
    bad <- is.na(x) | abs(as.vector(x)) == Inf
    ## rng <- extend.limits(range(as.numeric(x[!bad])))
    rng <- range(as.numeric(x[!bad]))
    tmp <- pretty(rng)
    att <- attributes(x)
    at <-
        structure(tmp, # [tmp >= rng[1] & tmp <= rng[2]],
                  format = att$format, 
                  origin = att$origin,
                  class = att$class)
    if (inherits(at, "chron")) 
        class(at) <- class(at)[-1]
    if (is.logical(labels)) ## labels unspecified
    {
        if (!inherits(x, "dates"))
        {
            at[c(1, length(at))] <- range(x)
            if (max(at) == 1) 
                labels <- format(at - trunc(at), simplify = simplify)
            else labels <- format(at, simplify = simplify)
        }
        else labels <- format(at, simplify = simplify)
    }
    ##invisible(list(n = n, at = at, labels = labels))

    list(at = as.numeric(at), labels = labels, 
         check.overlap = check.overlap,
         num.limit = if (length(x) == 2) as.numeric(x) else rng)
}

















panel.axis <-
    function(side = c("bottom", "left", "top", "right"),
             at = pretty(scale.range),
             labels = TRUE,
             draw.labels = TRUE,
             check.overlap = FALSE,
             outside = FALSE,
             ticks = TRUE,
             half = !outside, ## whether only half of the ticks will be labeled
             which.half = switch(side, bottom = "lower", left = "upper", top = "upper", right = "lower"),

             tck = as.numeric(ticks),
             rot = if (is.logical(labels)) 0 else c(90, 0),

             text.col = axis.text$col,
             text.alpha = axis.text$alpha,
             text.cex = axis.text$cex,
             text.font = axis.text$font,
             text.fontfamily = axis.text$fontfamily,
             text.fontface = axis.text$fontface,

             line.col = axis.line$col,
             line.lty = axis.line$lty,
             line.lwd = axis.line$lwd,
             line.alpha = axis.line$alpha)
{
    side <- match.arg(side)
    orientation <- if (outside) "outer" else "inner"
    cvp <- current.viewport() ## FIXME: grid should have accessors for xscale and yscale
    scale.range <-
        range(switch(side,
                     left = cvp$yscale,
                     top = cvp$xscale,
                     right = cvp$yscale,
                     bottom = cvp$xscale))

    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    rot <- rep(rot, length = 2) ## for x- and y-axes respectively

#    if (missing(at) || is.null(at))
#    {
#        
#        warning("nothing to draw if at not specified")
#        return()
#    }

    if (is.null(at) || length(at) == 0) return()

    ## get labels from at if unspecified
    if (is.logical(labels))
        labels <-
            if (labels) format(at, trim = TRUE)
            else NULL

    if (check.overlap) ## remove ticks close to limits
    {
        pad <- lattice.getOption("skip.boundary.labels")
        scale.range <- extend.limits(scale.range, prop = -pad)
    }

    ## skip ticks outside (or close to) axis limits
    keep.at <- at >= scale.range[1] & at <= scale.range[2]
    at <- at[keep.at]
    labels <- labels[keep.at]
    keep.labels <- TRUE

    ## could possibly treat ticks and labels separately, but that
    ## wasn't liked much I'll still keep 'keep.labels' around for now,
    ## but it's currently redundant

    ## if (check.overlap)
    ## {
    ##     pad <- lattice.getOption("something else")
    ##     scale.range <- extend.limits(scale.range, prop = -pad)
    ##     keep.labels <- at >= scale.range[1] & at <= scale.range[2]
    ## }

    nal <- length(at) / 2 + 0.5
    all.id <- seq(along = at)
    lower.id <- all.id <= nal
    upper.id <- all.id >= nal
    axid <-
        if (half)
        {
            if (which.half == "lower") lower.id else upper.id
        }
        else rep(TRUE, length(all.id))

    gp.line <- gpar(col = line.col, alpha = line.alpha,
                    lty = line.lty, lwd = line.lwd)
    gp.text <- gpar(col = text.col, cex = text.cex, alpha = text.alpha,
                    fontface = chooseFace(text.fontface, text.font),
                    fontfamily = text.fontfamily)

    ## We now compute some spacing information based on settings
    ## (combining trellis settings and the (newer) lattice.options).
    ## These can only be controlled via these settings and not by
    ## arguments to this function, for convenience for one thing, and
    ## also because the same settings will be used elsewhere to leave
    ## appropriate space.


    ## unit representing tick marks

    axis.units <- lattice.getOption("axis.units")[[orientation]][[side]]
    ## axis.units is of the form:
    ##     list(outer = list(left = list(tick=, pad1=, pad2=), top = list(...), ...),
    ##          inner = list(...) )
    axis.settings <- trellis.par.get("axis.components")[[side]]

    tck.unit.x <- tck * axis.settings$tck * axis.units$tick$x
    tck.unit <- unit(x = tck.unit.x, units = axis.units$tick$units)
    lab.unit <-
        if (tck.unit.x > 0) tck.unit + unit(x = axis.settings$pad1 * axis.units$pad1$x, units = axis.units$pad1$units)
        else unit(x = axis.settings$pad1 * axis.units$pad1$x, units = axis.units$pad1$units)
    orient.factor <- if (outside) -1 else 1

    if (tck.unit.x != 0)
        switch(side, 
               bottom = 
               grid.segments(x0 = unit(at[axid], "native"),
                             x1 = unit(at[axid], "native"),
                             y0 = unit(0, "npc"),
                             y1 = orient.factor * tck.unit,
                             gp = gp.line),
               top = 
               grid.segments(x0 = unit(at[axid], "native"),
                             x1 = unit(at[axid], "native"),
                             y0 = unit(1, "npc"),
                             y1 = unit(1, "npc") - orient.factor * tck.unit,
                             gp = gp.line),
               left = 
               grid.segments(y0 = unit(at[axid], "native"),
                             y1 = unit(at[axid], "native"),
                             x0 = unit(0, "npc"),
                             x1 = orient.factor * tck.unit,
                             gp = gp.line),
               right =
               grid.segments(y0 = unit(at[axid], "native"),
                             y1 = unit(at[axid], "native"),
                             x0 = unit(1, "npc"),
                             x1 = unit(1, "npc") - orient.factor * tck.unit,
                             gp = gp.line))

    if (draw.labels && !is.null(labels))
    {
        
        {
            just <-
                if (outside)
                    switch(side,
                           bottom = if (rot[1] == 0) c("centre", "top") else c("right", "centre"),
                           top = if (rot[1] == 0) c("centre", "bottom") else c("left", "centre"),
                           left = if (rot[2] == 90) c("centre", "bottom") else c("right", "centre"),
                           right = if (rot[2] == 90) c("centre", "top") else c("left", "centre"))
                else
                    switch(side,
                           bottom = if (rot[1] == 0) c("centre", "bottom") else c("left", "centre"),
                           top = if (rot[1] == 0) c("centre", "top") else c("right", "centre"),
                           left = if (rot[2] == 90) c("centre", "top") else c("left", "centre"),
                           right = if (rot[2] == 90) c("centre", "bottom") else c("right", "centre"))
        }
        switch(side,
               bottom =
               grid.text(label = labels[axid & keep.labels],
                         x = unit(at[axid & keep.labels], "native"),
                         y = orient.factor * lab.unit,
                         rot = rot[1],
                         check.overlap = check.overlap,
                         just = just,
                         gp = gp.text),
               top =
               grid.text(label = labels[axid & keep.labels],
                         x = unit(at[axid & keep.labels], "native"),
                         y = unit(1, "npc") - orient.factor * lab.unit,
                         rot = rot[1],
                         check.overlap = check.overlap,
                         just = just,
                         gp = gp.text),
               left =
               grid.text(label = labels[axid & keep.labels],
                         y = unit(at[axid & keep.labels], "native"),
                         x = orient.factor * lab.unit,
                         rot = rot[2],
                         check.overlap = check.overlap,
                         just = just,
                         gp = gp.text),
               right =
               grid.text(label = labels[axid & keep.labels],
                         y = unit(at[axid & keep.labels], "native"),
                         x = unit(1, "npc") - orient.factor * lab.unit,
                         rot = rot[2],
                         check.overlap = check.overlap,
                         just = just,
                         gp = gp.text))
    }
    invisible()
}






