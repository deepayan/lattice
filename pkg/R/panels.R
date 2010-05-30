

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

## the following functions don't do much error checking yet


panel.refline <- function(...) panel.abline(..., reference = TRUE)

panel.abline <-
    function(a = NULL, b = 0,
             h = NULL,
             v = NULL,
             reg = NULL,
             coef = NULL,
             col,
             col.line = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd,
             alpha = add.line$alpha,
             type, ...,
             reference = FALSE)
{
    add.line <- if (reference) trellis.par.get("reference.line") else trellis.par.get("add.line")
    if (!missing(col) && missing(col.line)) col.line <- col
    ## mostly copied from abline
    if (!is.null(reg))
    {
        if (!is.null(a))
            warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if (is.object(a) || is.list(a))
    {
        p <- length(coefa <- as.vector(coef(a)))
        if (p > 2)
            warning("only using the first two of ", p, "regression coefficients")
        islm <- inherits(a, "lm")
        noInt <- if (islm)
            !as.logical(attr(stats::terms(a), "intercept"))
        else p == 1
        if (noInt) {
            a <- 0
            b <- coefa[1]
        }
        else {
            a <- coefa[1]
            b <- if (p >= 2)
                coefa[2]
            else 0
        }
    }
    if (!is.null(coef))
    {
        if (!is.null(a))
            warning("'a' and 'b' are overridden by 'coef'")
        a <- coef[1]
        b <- coef[2]
    }
    ## draw y = a + bx if appropriate
    if (!is.null(a))
    {
        coeff <- c(a, b)
        cpl <- current.panel.limits()
        xx <- cpl$xlim
        yy <- cpl$ylim
        x <- numeric(0)
        y <- numeric(0)
        ll <- function(i, j, k, l)
            (yy[j]-coeff[1]-coeff[2]*xx[i]) *
                (yy[l]-coeff[1]-coeff[2]*xx[k])
        if (ll(1,1,2,1)<=0) {
            y <- c(y, yy[1])
            x <- c(x, (yy[1]-coeff[1])/coeff[2])
        }
        if (ll(2,1,2,2)<=0) {
            x <- c(x, xx[2])
            y <- c(y, coeff[1] + coeff[2] * xx[2])
        }
        if (ll(2,2,1,2)<=0) {
            y <- c(y, yy[2])
            x <- c(x, (yy[2]-coeff[1])/coeff[2])
        }
        if (ll(1,2,1,1)<=0) {
            x <- c(x, xx[1])
            y <- c(y, coeff[1] + coeff[2] * xx[1])
        }
        panel.lines(x = x, y = y, 
                    col = col.line,
                    lty = lty,
                    lwd = lwd,
                    alpha = alpha,
                    ...)
    }
    if (length(h <- as.numeric(h)) > 0)
        grid.segments(y0 = h, y1 = h, default.units="native",
                      gp = gpar(col = col.line, lty = lty, lwd = lwd, alpha = alpha))
    if (length(as.numeric(v)) > 0)
        grid.segments(x0 = v, x1 = v, default.units="native",
                      gp = gpar(col = col.line, lty = lty, lwd = lwd, alpha = alpha))
    invisible()
}





### old version of panel.abline

## panel.abline <- 
## function (a, b = NULL, h = numeric(0), v = numeric(0), col, col.line = add.line$col,
##     lty = add.line$lty, lwd = add.line$lwd, type, ...)
## {
##     add.line <- trellis.par.get("add.line")
##     if (!missing(col) && missing(col.line))
##         col.line <- col
##     if (!missing(a)) {
##         coeff <- if (inherits(a, "lm"))
##             coef(a)
##         else if (!is.null(coef(a)))
##             coef(a)
##         else c(a, b)
##         if (length(coeff) == 1)
##             coeff <- c(0, coeff)
##         if (coeff[2] == 0)
##             h <- c(h, coeff[1])
##         else if (!any(is.null(coeff))) {
##             xx <- current.viewport()$xscale
##             yy <- current.viewport()$yscale
##             x <- numeric(0)
##             y <- numeric(0)
##             ll <- function(i, j, k, l) (yy[j] - coeff[1] - coeff[2] *
##                 xx[i]) * (yy[l] - coeff[1] - coeff[2] * xx[k])
##             if (ll(1, 1, 2, 1) <= 0) {
##                 y <- c(y, yy[1])
##                 x <- c(x, (yy[1] - coeff[1])/coeff[2])
##             }
##             if (ll(2, 1, 2, 2) <= 0) {
##                 x <- c(x, xx[2])
##                 y <- c(y, coeff[1] + coeff[2] * xx[2])
##             }
##             if (ll(2, 2, 1, 2) <= 0) {
##                 y <- c(y, yy[2])
##                 x <- c(x, (yy[2] - coeff[1])/coeff[2])
##             }
##             if (ll(1, 2, 1, 1) <= 0) {
##                 x <- c(x, xx[1])
##                 y <- c(y, coeff[1] + coeff[2] * xx[1])
##             }
##             panel.lines(x = x, y = y, col = col.line, lty = lty,
##                 lwd = lwd, ...)
##         }
##     }
##     if (length(h <- as.numeric(h)))
##         grid.segments(y0 = h, y1 = h, default.units = "native",
##             gp = gpar(col = col.line, lty = lty, lwd = lwd))
##     if (length(as.numeric(v)))
##         grid.segments(x0 = v, x1 = v, default.units = "native",
##             gp = gpar(col = col.line, lty = lty, lwd = lwd))
## }







    

panel.curve <-
    function (expr, from, to, n = 101,
              curve.type = "l",
              col = add.line$col,
              lty = add.line$lty,
              lwd = add.line$lwd,
              type, ## ignored, to avoid type meant for panel.xyplot etc
              ...)
    ## curve has a log option. Unfortunately there is no easy way to
    ## read in the lattice log options (specified via scales) into the
    ## panel function. Maybe some day if grid natively supports log
    ## scales and lattice is redesigned to take advantage of that
{
    add.line <- trellis.par.get("add.line")
    sexpr <- substitute(expr)
    if (is.name(sexpr))
    {
        fcall <- paste(sexpr, "(x)")
        expr <- parse(text = fcall)
    }
    else
    {
        if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0)))
            stop("'expr' must be a function or an expression containing 'x'")
        expr <- sexpr
    }
    lims <- current.panel.limits()$xlim
    if (missing(from)) from <- min(lims)
    if (missing(to)) to <- max(lims)
    x <- seq(from, to, length.out = n)
    y <- eval(expr, envir = list(x = x), enclos = parent.frame())
    panel.lines(x, y, type = curve.type, col = col, lty = lty, lwd = lwd, ...)
}






panel.rug <-
    function(x = NULL, y = NULL,
             regular = TRUE,
             start = if (regular) 0 else 0.97,
             end = if (regular) 0.03 else 1,
             x.units = rep("npc", 2),
             y.units = rep("npc", 2),
             col = plot.line$col,
             lty = plot.line$lty,
             lwd = plot.line$lwd,
             alpha = plot.line$alpha,
             ...)
{
    if (!any(is.finite(x))) x <- NULL
    if (!any(is.finite(y))) y <- NULL
    plot.line <- trellis.par.get("plot.line")
    x.units <- rep(x.units, length.out = 2)
    y.units <- rep(y.units, length.out = 2)
    if (!is.null(x))
    {
        grid.segments(x0 = unit(x, "native"), x1 = unit(x, "native"),
                      y0 = unit(start, x.units[1]), y1 = unit(end, x.units[2]),
                      gp =
                      gpar(col = col, lty = lty,
                           lwd = lwd, alpha = alpha))
    }
    if (!is.null(y))
    {
        grid.segments(y0 = unit(y, "native"), y1 = unit(y, "native"),
                      x0 = unit(start, y.units[1]), x1 = unit(end, y.units[2]),
                      gp =
                      gpar(col = col, lty = lty,
                           lwd = lwd, alpha = alpha))
    }
}





panel.fill <-
    function(col = trellis.par.get("background")$col,
             border = "transparent", ...)
{
    grid.rect(gp =
              gpar(fill = col,
                   col = border,
                   ...))
}






panel.grid <-
    function(h = 3, v = 3,
             col,
             col.line = reference.line$col,
             lty = reference.line$lty,
             lwd = reference.line$lwd,
             x = NULL, y = NULL, ...)
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) col.line <- col
    h <- as.integer(h)
    v <- as.integer(v)

    if (h > 0)
        grid.segments(y0 = 1:h / (h+1),
                      y1 = 1:h / (h+1),
                      gp = gpar(col = col.line, lty = lty, lwd = lwd),
                      default.units = "npc",
                      name = trellis.grobname("panel.grid.h"))

    if (v > 0)
        grid.segments(x0 = 1:v / (v+1),
                      x1 = 1:v / (v+1),
                      gp = gpar(col = col.line, lty = lty, lwd = lwd),
                      default.units = "npc",
                      name = trellis.grobname("panel.grid.v"))

    ## Cheating here a bit for h=-1, v=-1. Can't think of any neat way to
    ## get the actual `at' values of the panel

    limits <- current.panel.limits()

    if (h < 0)
    {
        if (h == -1) n <- 5 else n <- -h
        scale <- limits$ylim
        if (!is.null(y)) {
            ## class() <- "factor" is an error
            if (inherits(y, "factor"))
                y <- as.character(y)
            mostattributes(scale) <- attributes(y)
        }
        #at <- pretty(scale, n = n) ## FIXME: use pretty eventually
        at <- formattedTicksAndLabels(scale, n = n)$at
        at <- at[at > min(scale) & at < max(scale)]
        grid.segments(y0 = at,
                      y1 = at,
                      gp = gpar(col = col.line, lty = lty, lwd = lwd),
                      default.units = "native",
                      name = trellis.grobname("panel.grid.h"))
    }
    if (v < 0)
    {
        if (v == -1) n <- 5 else n <- -v
        scale <- limits$xlim
        if (!is.null(x)) {
            ## class() <- "factor" is an error
            if (inherits(x, "factor"))
                x <- as.character(y)
            mostattributes(scale) <- attributes(x)
        }
        #at <- pretty(scale, n = n) ## FIXME: use pretty eventually
        at <- formattedTicksAndLabels(scale, n = n)$at
        at <- at[at > min(scale) & at < max(scale)]
        grid.segments(x0 = at,
                      x1 = at,
                      gp = gpar(col = col.line, lty = lty, lwd = lwd),
                      default.units = "native",
                      name = trellis.grobname("panel.grid.v"))
    }
}





panel.lmline <-
    function(x, y, ...)
{
    if (length(x) > 1) panel.abline(lm(as.numeric(y) ~ as.numeric(x)), ...)
}


prepanel.lmline <-
    function(x, y, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x) > 1) {
        coeff <- coef(lm(y~x))
        tem <- coeff[1] + coeff[2] * range(x, finite = TRUE)
        list(xlim = range(x, finite = TRUE),
             ylim = range(y, tem, finite = TRUE),
             dx = diff(range(x, finite = TRUE)),
             dy = diff(tem, finite = TRUE))
    }
    else prepanel.null()
}




panel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             lwd = plot.line$lwd,
             lty = plot.line$lty,
             col,
             col.line = plot.line$col,
             type, ## ignored
             horizontal = FALSE,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return()

    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal)
    {
        smooth <-
            loess.smooth(y[ok], x[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$y, y = smooth$x,
                    col = col.line, lty = lty, lwd = lwd, ...)
    }
    else
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$x, y = smooth$y,
                    col = col.line, lty = lty, lwd = lwd, ...)
    }
}



prepanel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) > 0)
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        list(xlim = range(x, smooth$x, finite = TRUE),
             ylim = range(y, smooth$y, finite = TRUE),
             dx = diff(smooth$x),
             dy = diff(smooth$y))
    }
    else prepanel.null()
}



# panel.smooth <-
#     function(x, y, span = 2/3, degree = 1, zero.line = FALSE,
#              family = c("symmetric", "gaussian"),
#              evaluation = 50,
#              lwd = plot.line$lwd, lty = plot.line$lty,
#              col = plot.line$col, ...)
# {
#     if (zero.line) abline(h=0, ...)
#     panel.loess(x, y, span = span, family = family,
#                 degree = degree, evaluation = evaluation, ...)
#     panel.xyplot(x, y, ...)
# }
## base R function exists




panel.superpose <-
    function(x, y = NULL, subscripts, groups,
             panel.groups = "panel.xyplot",
             ...,
             col = NA,
             col.line = superpose.line$col,
             col.symbol = superpose.symbol$col,
             pch = superpose.symbol$pch,
             cex = superpose.symbol$cex,
             fill = superpose.symbol$fill,
             font = superpose.symbol$font,
             fontface = superpose.symbol$fontface,
             fontfamily = superpose.symbol$fontfamily,
             lty = superpose.line$lty,
             lwd = superpose.line$lwd,
             alpha = superpose.symbol$alpha,
             type = 'p',
             distribute.type = FALSE)
{
    if (distribute.type)
    {

        ## This implies a slightly different behaviour: the 'type'
        ## argument behaves like other graphical parameters, i.e., it
        ## is repeated to be as long as the number of groups, and one
        ## used for each group.  This is the default behaviour of
        ## panel.superpose in S-PLUS.  The lattice default
        ## (!distribute.type) is to use all type values concurrently
        ## for each group.  We accomplish this by transforming 'type'
        ## to a list in either case (but in different ways) and then
        ## use common code.

        ## have.type <- FALSE
        type <- as.list(type)
    }
    else
    {
        ## this is something of a hack, needed because without this,
        ## grouped displays with 'g' %in% type may draw a grid
        ## (courtesy of panel.groups) for each group, overwriting
        ## earlier ones.

        ## have.type <- TRUE
        type <- unique(type)
        wg <- match('g', type, nomatch = NA_character_)
        if (!is.na(wg))
        {
            panel.grid(h = -1, v = -1, x = x, y = y)
            type <- type[-wg]
        }
        type <- list(type)
    }
    x <- as.numeric(x)
    if (!is.null(y)) y <- as.numeric(y)
    if (length(x) > 0)
    {
        if (!missing(col))
        {
            if (missing(col.line)) col.line <- col
            if (missing(col.symbol)) col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        vals <-
            if (is.factor(groups)) levels(groups)
            else sort(unique(groups))
        nvals <- length(vals)
        col <- rep(col, length.out = nvals)
        col.line <- rep(col.line, length.out = nvals)
        col.symbol <- rep(col.symbol, length.out = nvals)
        pch <- rep(pch, length.out = nvals)
        fill <- rep(fill, length.out = nvals)
        lty <- rep(lty, length.out = nvals)
        lwd <- rep(lwd, length.out = nvals)
        alpha <- rep(alpha, length.out = nvals)
        cex <- rep(cex, length.out = nvals)
        font <- rep(font, length.out = nvals)
        fontface <- rep(fontface, length.out = nvals)
        fontfamily <- rep(fontfamily, length.out = nvals)
        type <- rep(type, length.out = nvals)

        panel.groups <-
            if (is.function(panel.groups)) panel.groups
            else if (is.character(panel.groups)) get(panel.groups)
            else eval(panel.groups)

        subg <- groups[subscripts]
        ok <- !is.na(subg)
        for (i in seq_along(vals))
        {
            id <- ok & (subg == vals[i])
            if (any(id))
            {
                args <-
                    list(x = x[id],
                         ## groups = groups,
                         subscripts = subscripts[id],
                         pch = pch[i], cex = cex[i],
                         font = font[i],
                         fontface = fontface[i],
                         fontfamily = fontfamily[i],
                         col = col[i],
                         col.line = col.line[i],
                         col.symbol = col.symbol[i],
                         fill = fill[i],
                         lty = lty[i],
                         lwd = lwd[i],
                         alpha = alpha[i],
                         type = type[[i]],
                         group.number = i,
                         group.value = vals[i],
                         ...)
                ## if (have.type) args$type <- type
                if (!is.null(y)) args$y <- y[id]
                do.call(panel.groups, args)
            }
        }
    }
}



panel.superpose.2 <-
    function(..., distribute.type = TRUE)
{
    panel.superpose(...,
                    distribute.type = distribute.type)
}


## panel.superpose.2 <-
##     function(x, y = NULL, subscripts, groups,
##              panel.groups = "panel.xyplot",
##              col,
##              col.line = superpose.line$col,
##              col.symbol = superpose.symbol$col,
##              pch = superpose.symbol$pch,
##              cex = superpose.symbol$cex, 
##              font = superpose.symbol$font, 
##              fontface = superpose.symbol$fontface, 
##              fontfamily = superpose.symbol$fontfamily, 
##              lty = superpose.line$lty,
##              lwd = superpose.line$lwd,
##              alpha = superpose.symbol$alpha,
##              type = "p",
##              ...)
## {

##     ## This is a slightly different version of panel.superpose.  It
##     ## has an explicit type argument which behaves like other
##     ## graphical parameters, i.e., it is repeated to be as long as the
##     ## number of groups, and one used for each group.  This is the
##     ## default behaviour of panel.superpose in S-PLUS.

##     ## Original version contributed by Neil Klepeis

##     type <- as.list(type)

##     x <- as.numeric(x)
##     if (!is.null(y)) y <- as.numeric(y)

##     if (length(x)>0)
##     {
##         if (!missing(col))
##         {
##             if (missing(col.line)) col.line <- col
##             if (missing(col.symbol)) col.symbol <- col
##         }

##         superpose.symbol <- trellis.par.get("superpose.symbol")
##         superpose.line <- trellis.par.get("superpose.line")

##         vals <-
##             if (is.factor(groups)) levels(groups)
##             else sort(unique(groups))
##         nvals <- length(vals)
##         col.line <- rep(col.line, length.out = nvals)
##         col.symbol <- rep(col.symbol, length.out = nvals)
##         pch <- rep(pch, length.out = nvals)
##         lty <- rep(lty, length.out = nvals)
##         lwd <- rep(lwd, length.out = nvals)
##         alpha <- rep(alpha, length.out = nvals)
##         cex <- rep(cex, length.out = nvals)
##         font <- rep(font, length.out = nvals)
##         fontface <- rep(fontface, length.out = nvals)
##         fontfamily <- rep(fontfamily, length.out = nvals)
##         type <- rep(type, length.out = nvals)

##         panel.groups <- 
##             if (is.function(panel.groups)) panel.groups
##             else if (is.character(panel.groups)) get(panel.groups)
##             else eval(panel.groups)

##         subg <- groups[subscripts]
##         ok <- !is.na(subg)
##         for (i in seq_along(vals))
##         {
##             id <- ok & (subg == vals[i])
##             if (any(id))
##             {
##                 args <-
##                     list(x=x[id],
##                          groups = groups,
##                          subscripts = subscripts[id],
##                          pch = pch[i], cex = cex[i],
##                          font = font[i],
##                          fontface = fontface[i],
##                          fontfamily = fontfamily[i],
##                          col.line = col.line[i],
##                          col.symbol = col.symbol[i],
##                          lty = lty[i],
##                          lwd = lwd[i],
##                          alpha = alpha[i],
##                          type = type[[i]], ...)
##                 if (!is.null(y)) args$y <- y[id]

##                 do.call(panel.groups, args)
##             }
##         }
##     }
## }





# panel.superpose.2 <- 
#     function(x, y, subscripts, groups,
#              col, col.line = superpose.line$col,
#              col.symbol = superpose.symbol$col,
#              pch = superpose.symbol$pch,
#              cex = superpose.symbol$cex,
#              lty = superpose.line$lty,
#              lwd = superpose.line$lwd, type="p", ...)
# {
    
#     ## `panel.superpose.2' : This is a version of the
#     ## 'panel.superpose' Trellis panel function that allows the plot
#     ## `type' to change between superimposed (overlayed) data sets.
#     ## See the `panel.xyplot' function for details on the `type'
#     ## option which is usually a single character, but here is a
#     ## character vector with each element specifying the plot style of
#     ## each subsequently-overlayed plot.  --- Neil Klepeis,
#     ## 26-Dec-2001
    
#     x <- as.numeric(x)
#     y <- as.numeric(y)

#     if (length(x) > 0) {
#         if (!missing(col)) {
#             if (missing(col.line))
#                 col.line <- col
#             if (missing(col.symbol))
#                 col.symbol <- col
#         }
#         superpose.symbol <- trellis.par.get("superpose.symbol")
#         superpose.line <- trellis.par.get("superpose.line")
#         x <- as.numeric(x)
#         y <- as.numeric(y)
#         vals <-
#             if (is.factor(groups)) levels(groups)
#             else sort(unique(groups))
#         nvals <- length(vals)
#         col.line <- rep(col.line, length.out = nvals)
#         col.symbol <- rep(col.symbol, length.out = nvals)
#         pch <- rep(pch, length.out = nvals)
#         lty <- rep(lty, length.out = nvals)
#         lwd <- rep(lwd, length.out = nvals)
#         cex <- rep(cex, length.out = nvals)
#         type <- rep(type, length.out = nvals)      # new line here
#         for (i in seq_along(vals)) {
#             id <- (groups[subscripts] == vals[i])
#             if (any(id))
#                 panel.xyplot(x = x[id], y = y[id], pch = pch[i],
#                   cex = cex[i], col.line = col.line[i], col.symbol = col.symbol[i],
#                   lty = lty[i], lwd = lwd[i], type=type[i], ...)
#         }
#     }
# }







panel.linejoin <-
panel.average <-
    function(x, y, fun = mean,
             horizontal = TRUE,
             lwd = reference.line$lwd,
             lty = reference.line$lty,
             col,
             col.line = reference.line$col,
             type = "l", ## ignored
             ...)
{
    ## FIXME: pretty sure this can be made more readable using tapply
    x <- as.numeric(x)
    y <- as.numeric(y)

    reference.line = trellis.par.get("reference.line")
    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
    }
    if (horizontal)
    {
        vals <- unique(sort(y))
        yy <- seq_along(vals)
        xx <- numeric(length(yy))
        for (i in yy)
            xx[i] <- fun(x[y == vals[i]])
        panel.lines(xx, vals[yy], col = col.line, lty = lty, lwd = lwd, ...)
    }
    else
    {
        vals <- unique(sort(x))
        xx <- seq_along(vals)
        yy <- numeric(length(xx))
        for (i in xx)
            yy[i] <- fun(y[x == vals[i]])
        panel.lines(vals[xx], yy, col = col.line, lty = lty, lwd = lwd, ...)
     }
}



panel.mathdensity <-
    function(dmath = dnorm,
             args = list(mean = 0, sd = 1),
             n = 50,
             col,
             col.line = reference.line$col,
             lwd = reference.line$lwd,
             lty = reference.line$lty,
             type,
             ...)
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) col.line <- col
    x <- do.breaks(endpoints = current.panel.limits()$xlim, nint = n)
    y <- do.call("dmath", c(list(x = x), args))
    panel.lines(x = x, y = y, col = col.line, lwd = lwd, lty = lty, ...)
}

