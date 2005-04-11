

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

## the foll functions don't do much error checking yet



panel.abline <-
    function(a, b = NULL, h = numeric(0), v = numeric(0),
             col, col.line = add.line$col, lty = add.line$lty,
             lwd = add.line$lwd, ...)
{
    add.line <- trellis.par.get("add.line")
    if (!missing(col)) {
        if (missing(col.line)) col.line <- col
    }
    
    if (!missing(a)) {
        if (inherits(a,"lm")) {
            coeff <- coef(a)
        }
        else if (!is.null(coef(a))) coeff <- coef(a)  # ????
        else coeff <- c(a,b)

        if (length(coeff)==1) coeff <- c(0, coeff)
        
        if (coeff[2]==0) h <- c(h, coeff[1])
        else if (!any(is.null(coeff))) {
            xx <- current.viewport()$xscale
            yy <- current.viewport()$yscale
            
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
            
            if (length(x)>0)
                grid.lines(x=x, y = y, default.units="native",
                           gp = gpar(col=col.line, lty=lty, lwd=lwd))
        }
    }
    
    h <- as.numeric(h)
    v <- as.numeric(v)
    
    for(i in seq(along=h))
        grid.lines(y=rep(h[i],2), default.units="native", gp = gpar(col=col.line,lty=lty,lwd=lwd))

    for(i in seq(along=v))
        grid.lines(x=rep(v[i],2), default.units="native", gp = gpar(col=col.line,lty=lty,lwd=lwd))
    
}




panel.curve <-
    function (expr, from, to, n = 101,
              curve.type = "l",
              col = add.line$col,
              lty = add.line$lty,
              lwd = add.line$lwd,
              type = NULL, ## avoid type meant for panel.xyplot etc
              ...)
    ## curve has a log option. Unfortunately there is no easy way to
    ## read in the lattice log options (specified via scales) into the
    ## panel function. Maybe some day if grid natively supports log
    ## scales and lattice is redesigned to take advantage of that
{
    add.line <- trellis.par.get("add.line")
    sexpr <- substitute(expr)
    if (is.name(sexpr)) {
        fcall <- paste(sexpr, "(x)")
        expr <- parse(text = fcall)
    }
    else {
        if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0))) 
            stop("'expr' must be a function or an expression containing 'x'")
        expr <- sexpr
    }
    lims <- current.viewport()$xscale
    if (missing(from)) 
        from <- lims[1]
    if (missing(to)) 
        to <- lims[2]
    x <- seq(from, to, length = n)
    y <- eval(expr, envir = list(x = x), enclos = parent.frame())
    llines(x, y, type = curve.type, col = col, lty = lty, lwd = lwd, ...)
}






panel.rug <-
    function(x = NULL, y = NULL,
             regular = TRUE, 
             start = if (regular) 0 else 0.97,
             end = if (regular) 0.03 else 1,
             col = add.line$col,
             ...)
{
    add.line <- trellis.par.get("add.line")
    if (!is.null(x))
    {
        grid.segments(x0 = unit(x, "native"), x1 = unit(x, "native"),
                      y0 = unit(start, "npc"), y1 = unit(end, "npc"),
                      gp = gpar(col = col))
    }
    if (!is.null(y))
    {
        grid.segments(y0 = unit(y, "native"), y1 = unit(y, "native"),
                      x0 = unit(start, "npc"), x1 = unit(end, "npc"),
                      gp = gpar(col = col))
    }
}





panel.fill <-
    function(col = trellis.par.get("background")$col, ...)
{
    grid.rect(gp=gpar(fill=col))
}












panel.grid <-
    function(h = 3, v = 3,
             col, col.line = reference.line$col,
             lty = reference.line$lty,
             lwd = reference.line$lwd, ...)
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col)) {
        if (missing(col.line)) col.line <- col
    }

    if (h>0)
        for(i in 1:h)
            grid.lines(y=rep(i/(h+1),2),
                       gp = gpar(col = col.line, lty = lty, lwd = lwd),
                       default.units="npc")

    if (v>0)
        for(i in 1:v)
            grid.lines(x=rep(i/(v+1),2),
                       gp = gpar(col = col.line, lty = lty, lwd = lwd),
                       default.units="npc")


    ## Cheating here a bit for h=-1, v=-1. Can't think of any neat way to
    ## get the actual `at' values of the panel (Can pass it in though)

    if (h<0)
    {
        scale <- current.viewport()$yscale
        at <- pretty(scale)
        at <- at[at>scale[1] & at < scale[2]]
        for(i in seq(along=at))
            grid.lines(y=rep(at[i],2), default.units="native",
                       gp = gpar(col = col.line, lty = lty, lwd = lwd))
    }
    if (v<0)
    {
        scale <- current.viewport()$xscale
        at <- pretty(scale)
        at <- at[at>scale[1] & at < scale[2]]
        for(i in seq(along=at))
            grid.lines(x=rep(at[i],2), default.units="native",
                       gp = gpar(col = col.line, lty = lty, lwd = lwd))
    }
}





panel.lmline <-
    function(x, y, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (length(x)>0) panel.abline(lm(y ~ x), ...) 
}


prepanel.lmline <-
    function(x, y, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x)>0) {
        coeff <- coef(lm(y~x))
        tem <- coeff[1] + coeff[2] * range(x)
        list(xlim=range(x), ylim=range(y,tem), 
             dx=diff(range(x)), dy=diff(tem))         
    }
    else list(xlim=c(NA,NA), ylim=c(NA,NA), dx=NA, dy=NA)
}










panel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             lwd = add.line$lwd, lty = add.line$lty,
             col,
             col.line = add.line$col,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x)>0) {

        if (!missing(col)) {
            if (missing(col.line)) col.line <- col
        }

        add.line <- trellis.par.get("add.line")
        
        smooth <- loess.smooth(x, y, span = span, family = family,
                               degree = degree, evaluation = evaluation)
        grid.lines(x=smooth$x, y=smooth$y, default.units = "native",
                   gp = gpar(col = col.line, lty = lty, lwd = lwd))
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

    if (length(x)>0) {
        smooth <-
            loess.smooth(x, y, span = span, family = family,
                         degree = degree, evaluation = evaluation)
        list(xlim = range(x,smooth$x),
             ylim = range(y,smooth$y),
             dx = diff(smooth$x),
             dy = diff(smooth$y))
    }
    else list(xlim=c(NA,NA), ylim=c(NA,NA), dx=NA, dy=NA)
}



# panel.smooth <-
#     function(x, y, span = 2/3, degree = 1, zero.line = FALSE,
#              family = c("symmetric", "gaussian"),
#              evaluation = 50,
#              lwd = add.line$lwd, lty = add.line$lty,
#              col = add.line$col, ...)
# {
#     if (zero.line) abline(h=0, ...)
#     panel.loess(x, y, span = span, family = family,
#                 degree = degree, evaluation = evaluation, ...)
#     panel.xyplot(x, ,y, ...)
# }
## base R function exists




panel.superpose <-
    function(x, y = NULL, subscripts, groups,
             panel.groups = "panel.xyplot",
             col,
             col.line = superpose.line$col,
             col.symbol = superpose.symbol$col,
             pch = superpose.symbol$pch,
             cex = superpose.symbol$cex, 
             font = superpose.symbol$font, 
             fontface = superpose.symbol$fontface, 
             fontfamily = superpose.symbol$fontfamily, 
             lty = superpose.line$lty,
             lwd = superpose.line$lwd,
             ...)
{
    x <- as.numeric(x)
    if (!is.null(y)) y <- as.numeric(y)

    if (length(x)>0)
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
        col.line <- rep(col.line, length=nvals)
        col.symbol <- rep(col.symbol, length=nvals)
        pch <- rep(pch, length=nvals)
        lty <- rep(lty, length=nvals)
        lwd <- rep(lwd, length=nvals)
        cex <- rep(cex, length=nvals)
        font <- rep(font, length=nvals)
        fontface <- rep(fontface, length=nvals)
        fontfamily <- rep(fontfamily, length=nvals)

        panel.groups <- 
            if (is.function(panel.groups)) panel.groups
            else if (is.character(panel.groups)) get(panel.groups)
            else eval(panel.groups)

        for (i in seq(along=vals))
        {
            id <- (groups[subscripts] == vals[i])
            if (any(id)) {
                args <- list(x=x[id],
                             groups = groups,
                             subscripts = subscripts[id],
                             pch = pch[i], cex = cex[i],
                             font = font[i],
                             fontface = fontface[i],
                             fontfamily = fontfamily[i],
                             col.line = col.line[i],
                             col.symbol = col.symbol[i],
                             lty = lty[i],
                             lwd = lwd[i], ...)
                if (!is.null(y)) args$y <- y[id]

                do.call("panel.groups", args)
            }
        }
    }
}




panel.superpose.2 <-
    function(x, y = NULL, subscripts, groups,
             panel.groups = "panel.xyplot",
             col,
             col.line = superpose.line$col,
             col.symbol = superpose.symbol$col,
             pch = superpose.symbol$pch,
             cex = superpose.symbol$cex, 
             font = superpose.symbol$font, 
             fontface = superpose.symbol$fontface, 
             fontfamily = superpose.symbol$fontfamily, 
             lty = superpose.line$lty,
             lwd = superpose.line$lwd,
             type = "p",
             ...)
{

    ## This is a (very) slightly different version of panel.superpose.
    ## It has an explicit type argument which behaves like other
    ## graphical parameters, i.e., it is repeated to be as long as the
    ## number of groups, and one used for each group.  This is the
    ## default behaviour of panel.superpose in S-PLUS.

    ## Original version contributed by Neil Klepeis

    type <- as.list(type)

    x <- as.numeric(x)
    if (!is.null(y)) y <- as.numeric(y)

    if (length(x)>0)
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
        col.line <- rep(col.line, length=nvals)
        col.symbol <- rep(col.symbol, length=nvals)
        pch <- rep(pch, length=nvals)
        lty <- rep(lty, length=nvals)
        lwd <- rep(lwd, length=nvals)
        cex <- rep(cex, length=nvals)
        font <- rep(font, length=nvals)
        fontface <- rep(fontface, length=nvals)
        fontfamily <- rep(fontfamily, length=nvals)
        type <- rep(type, length=nvals)

        panel.groups <- 
            if (is.function(panel.groups)) panel.groups
            else if (is.character(panel.groups)) get(panel.groups)
            else eval(panel.groups)

        for (i in seq(along=vals))
        {
            id <- (groups[subscripts] == vals[i])
            if (any(id))
            {
                args <- list(x=x[id],
                             groups = groups,
                             subscripts = subscripts[id],
                             pch = pch[i], cex = cex[i],
                             font = font[i],
                             fontface = fontface[i],
                             fontfamily = fontfamily[i],
                             col.line = col.line[i],
                             col.symbol = col.symbol[i],
                             lty = lty[i],
                             lwd = lwd[i],
                             type = type[[i]], ...)
                if (!is.null(y)) args$y <- y[id]

                do.call("panel.groups", args)
            }
        }
    }
}





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
#         col.line <- rep(col.line, length = nvals)
#         col.symbol <- rep(col.symbol, length = nvals)
#         pch <- rep(pch, length = nvals)
#         lty <- rep(lty, length = nvals)
#         lwd <- rep(lwd, length = nvals)
#         cex <- rep(cex, length = nvals)
#         type <- rep(type, length = nvals)      # new line here
#         for (i in seq(along = vals)) {
#             id <- (groups[subscripts] == vals[i])
#             if (any(id))
#                 panel.xyplot(x = x[id], y = y[id], pch = pch[i],
#                   cex = cex[i], col.line = col.line[i], col.symbol = col.symbol[i],
#                   lty = lty[i], lwd = lwd[i], type=type[i], ...)
#         }
#     }
# }







panel.linejoin <-
    function(x, y, fun = mean,
             horizontal = TRUE,
             lwd = reference.line$lwd,
             lty = reference.line$lty,
             col,
             col.line = reference.line$col,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    reference.line = trellis.par.get("reference.line")
    if (!missing(col)) {
        if (missing(col.line)) col.line <- col
    }
    if (horizontal) {
        vals <- unique(sort(y))
        yy <- seq(along = vals)
        xx <- numeric(length(yy))
        for (i in yy)
            xx[i] <- fun(x[y == vals[i]])
        llines(xx, vals[yy], col = col.line, lty = lty, lwd = lwd, ...)
    }
    else {
        vals <- unique(sort(x))
        xx <- seq(along = vals)
        yy <- numeric(length(xx))
        for (i in xx)
            yy[i] <- fun(y[x == vals[i]])
        llines(vals[xx], yy, col = col.line, lty = lty, lwd = lwd, ...)
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
             ...)
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col)) {
        if (missing(col.line)) col.line <- col
    }
    x <- do.breaks(endpoints = current.viewport()$xscale,
                   nint = n)
    y <- do.call("dmath", c(list(x = x),args))
    llines(x = x, y = y, col = col.line, lwd = lwd, lty = lty, ...)
}


