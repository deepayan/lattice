


### Copyright 2001-2004 Deepayan Sarkar <deepayan@stat.wisc.edu> and 
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




chooseFace <- function(fontface = NULL, font = 1)
    if (is.null(fontface)) font else fontface



lpretty <- function(x, ...) { 
    eps <- 1e-10
    at <- pretty(x[is.finite(x)], ...)
    ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
}


oneway <-
    function(formula, data, location = mean,
             spread = function(x) sqrt(var(x)))
{
    if(missing(data)) data <- sys.frame(sys.parent())
    form <- latticeParseFormula(formula, data)
    y <- form$left
    x <- form$right
    if (!is.shingle(x)) x <- as.factor(x)
    is.f.x <- is.factor(x)
    num.l.x <- nlevels(x) 
    foo <- list()
    if (is.f.x) {
        foo$location <-
            if (is.function(location)) as.vector(tapply(X=y, INDEX=list(x), FUN = location))
            else rep(location, num.l.x)
        foo$spread <- 
            if (is.function(spread)) as.vector(tapply(X=y, INDEX=list(x), FUN = spread))
            else rep(spread, num.l.x)
        foo$fitted.values <- numeric(length(y))
        sc <- numeric(length(y))
        for (i in seq(along = y)){
            foo$fitted.values[i] <- foo$location[as.numeric(x)[i]]
            sc[i] <- foo$spread[as.numeric(x)[i]]
        }
        foo$residuals <- y - foo$fitted.values
        foo$scaled.residuals <- foo$residuals/sc
    }
    else stop("x must be (coercible to be) a factor")
    foo
}



is.characterOrExpression <- function(x)
    is.character(x) || is.expression(x)




## This converts character to factor, numeric to shingle, and
## in addition, takes subsets
as.factorOrShingle <- function(x, subset = TRUE, drop = FALSE)
{
    x <-
        if (is.numeric(x))
            as.shingle(x)
        else ##if (is.character(x)) or logical or ??
            as.factor(x)
    x[subset, drop = drop]
}




## update elements of a list recursively. Used in updating trellis or
## lattice settings using trellis.par.set and lattice.options
## respectively

updateList <-
    function(x, val)
{
    if (is.null(x)) x <- list()
    if (!is.list(x)) stop("x must be NULL or a list")
    if (!is.list(val)) stop("val must be a list")
    xnames <- names(x)
    for (v in names(val))
    {
        existing <- v %in% xnames
        if (existing && is.list(x[[v]]) && is.list(val[[v]]))
            x[[v]] <- updateList(x[[v]], val[[v]])
        else 
            x[[v]] <- val[[v]]
    }
    x
}






## Next 3 are convenience functions following those available in Trellis

do.breaks  <- function(endpoints, nint)
{
    if (length(endpoints)!=2) stop("error")
    endpoints[1] + diff(endpoints) * 0:nint / nint
}


Rows <- function(x, which)
{
    for (i in seq(along = x)) x[[i]] <-
        rep(x[[i]], length = max(which, length(which)))[which]
    x
}


reorderFactor <-
    function(x, X, FUN = mean,
             ..., 
             order = is.ordered(x))
    (if (order) ordered else factor)(x,
                                     levels = names(sort(tapply(X,
                                     x, FUN, ...))))




## panel functions corresponding to standard base functions

panel.points <- function(...) lpoints(...)
panel.lines <- function(...) llines(...)
panel.segments <- function(...) lsegments(...)
panel.text <- function(...) ltext(...)
panel.arrows <- function(...) larrows(...)







## The rest are grid-ified versions of standard base 'incremental
## graphics' functions.  Maybe it's better to push wrappers like
## panel.points, panel.lines, etc.



lsegments <-
    function(x0 = NULL, y0 = NULL, x1, y1,
             x2 = NULL, y2 = NULL,
             col = add.line$col,
             alpha = add.line$alpha,
             lty = add.line$lty,
             lwd = add.line$lwd, ...)
{
    if (missing(x0)) x0 <- x2
    if (missing(y0)) y0 <- y2
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length = ml)
    x1 <- rep(x1, length = ml)
    y0 <- rep(y0, length = ml)
    y1 <- rep(y1, length = ml)
    grid.segments(x0 = x0, x1 = x1,
                  y0 = y0, y1 = y1,
                  gp = gpar(lty=lty,
                  col = col, lwd = lwd, alpha = alpha),
                  default.units = "native")
}


larrows <-
    function(x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL,
             angle = 30, code = 2, length = NULL, proportion = .05, ...) 
{

    if (missing(x0)) {x0 <- x1; x1 <- x2}
    if (missing(y0)) {y0 <- y1; y1 <- y2}
    if (!is.null(length))
        warning("length not implemented in larrows, use proportion instead")

    angle <- angle / 180 * pi
    start <- rbind(x0, y0)
    end <- rbind(x1, y1)
    v.forward <- end - start
    v.backward <- start - end
    lsegments(x0, y0, x1, y1, ...)
    
    if (code %in% c(1,3)) { # arrow at starting point
        edge.1 <- proportion * 
            matrix( c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2) %*% v.forward
        edge.2 <- proportion *
            matrix( c(cos(-angle), -sin(-angle), sin(-angle), cos(-angle)), 2, 2) %*% v.forward
        lsegments(x0, y0, x0 + edge.1[1,], y0 + edge.1[2,], ...)
        lsegments(x0, y0, x0 + edge.2[1,], y0 + edge.2[2,], ...)
    }
    if (code %in% c(2,3)) { # arrow at ending point
        edge.1 <- proportion * 
            matrix( c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2) %*% v.backward
        edge.2 <- proportion *
            matrix( c(cos(-angle), -sin(-angle), sin(-angle), cos(-angle)), 2, 2) %*% v.backward
        lsegments(x1, y1, x1 + edge.1[1,], y1 + edge.1[2,], ...)
        lsegments(x1, y1, x1 + edge.2[1,], y1 + edge.2[2,], ...)
    }
}



ltext <-
    function(x, y = NULL, labels = seq(along = x),
             col = add.text$col,
             alpha = add.text$alpha,
             cex = add.text$cex,
             srt = 0,
             font = add.text$font,
             fontfamily = add.text$fontfamily,
             fontface = add.text$fontface,
             adj = c(.5, .5),
             pos = NULL,
             offset = 0.5,
             ## FIXME: need an offset argument
             ...)
{
    add.text <- trellis.par.get("add.text")
    xy <- xy.coords(x, y)
    if (length(xy$x) == 0) return()
    ux <- unit(xy$x, "native")
    uy <- unit(xy$y, "native")
    if (!is.null(pos))
    {
        if (pos == 1) {
            uy <- uy - unit(offset, "char")
            adj <- c(.5, 1)
        }
        else if (pos == 2) {
            ux <- ux - unit(offset, "char")
            adj <- c(1, .5)
        }
        else if (pos == 3) {
            uy <- uy + unit(offset, "char")
            adj <- c(.5, 0)
        }
        else if (pos == 4) {
            ux <- ux + unit(offset, "char")
            adj <- c(0, .5)
        }
        else stop("Invalid value of pos")
        
    }
    if (length(adj) == 1) adj <- c(adj, .5)
    grid.text(label = labels, x = ux, y = uy,
              gp =
              gpar(col = col, alpha = alpha, 
                   fontfamily = fontfamily,
                   fontface = chooseFace(fontface, font),
                   cex = cex),
              just = c(if (adj[1] == 0) "left"
              else if (adj[1] == 1) c("right")
              else "centre",
              if (adj[2] == 0) "bottom"
              else if (adj[2] == 1) c("top")
              else "centre"),
              rot = srt)
}





llines <-
    function(x, y = NULL, type = "l",
             col = plot.line$col,
             alpha = plot.line$alpha,
             lty = plot.line$lty,
             lwd = plot.line$lwd, ...)
{
    plot.line <- trellis.par.get("plot.line")
    lplot.xy(xy.coords(x, y), type = type,
             col = col, lty = lty, lwd = lwd, alpha = alpha, ...)
}




lpoints <-
    function(x, y = NULL, type = "p",
             col = plot.symbol$col,
             pch = plot.symbol$pch,
             alpha = plot.symbol$alpha,
             font = plot.symbol$font,
             fontfamily = plot.symbol$fontfamily,
             fontface = plot.symbol$fontface,
             cex = plot.symbol$cex, ...)
{
    plot.symbol <- trellis.par.get("plot.symbol")
    lplot.xy(xy.coords(x, y), type = type,
             col = col, pch = pch, font = font,
             fontfamily = fontfamily, fontface = fontface,
             cex = cex, alpha = alpha, ...)
}






lplot.xy <-
    function(xy, type, pch = 1, lty = 1, col = 1, cex = 1, lwd = 1,
             font = 1, fontfamily = NULL, fontface = NULL,
             col.line = col, alpha = 0,
             ...)
{
    x <- xy$x
    y <- xy$y
    fontsize.points <- trellis.par.get("fontsize")$points

    if (length(x) == 0) return()
    else if (type %in% c("l", "o", "b", "c"))
        grid.lines(x = x, y = y,
                   gp = gpar(lty = lty, col = col.line, lwd = lwd, alpha = alpha),
                   default.units = "native")
    else if (type %in% c("p", "o", "b", "c"))
        grid.points(x = x, y = y, 
                    gp =
                    gpar(col = col, cex = cex, alpha = alpha,
                         fontsize = fontsize.points,
                         fontfamily = fontfamily,
                         fontface = chooseFace(fontface, font)),
                    pch = pch, 
                    default.units = "native")
    else if (type %in% c("s", "S"))
    {
        ord <- sort.list(x)
        n <- length(x)
        xx <- numeric(2*n-1)
        yy <- numeric(2*n-1)
        xx[2*1:n-1] <- x[ord]
        yy[2*1:n-1] <- y[ord]
        xx[2*1:(n-1)] <- x[ord][if (type=="s") -1 else -n]
        yy[2*1:(n-1)] <- y[ord][if (type=="s") -n else -1]
        grid.lines(x=xx, y=yy,
                   gp = gpar(lty=lty, col=col.line, lwd=lwd, alpha = alpha),
                   default.units="native")
    }
    else if (type == "h")
    {
        ylim <- current.viewport()$yscale
        zero <-
            if (ylim[1] > 0) ylim[1]
            else if (ylim[2] < 0) ylim[2]
            else 0
        for (i in seq(along=x))
            grid.lines(x = rep(x[i],2), y = c(y[i], zero),
                       gp = gpar(lty = lty, col = col.line, lwd = lwd, alpha = alpha),
                       default.units = "native")
    }
    else if (type == "h")
    {
        ylim <- current.viewport()$yscale
        zero <-
            if (ylim[1] > 0) ylim[1]
            else if (ylim[2] < 0) ylim[2]
            else 0
        grid.segments(x0 = x, x1 = x,
                      y0 = y, y1 = zero,
                      gp = gpar(lty = lty, col = col.line, lwd = lwd, alpha = alpha),
                      default.units="native")
    }
    else if (type == "H")
    {
        xlim <- current.viewport()$xscale
        zero <-
            if (xlim[1] > 0) xlim[1]
            else if (xlim[2] < 0) xlim[2]
            else 0
        grid.segments(x0 = x, x1 = zero,
                      y0 = y, y1 = y,
                      gp = gpar(lty = lty, col = col.line, lwd = lwd, alpha = alpha),
                      default.units="native")
    }
    return()
}


