


### Copyright 2001-2003 Deepayan Sarkar <deepayan@stat.wisc.edu> and 
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
    at <- ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
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


do.breaks  <- function(endpoints, nint)
{
    if (length(endpoints)!=2) stop("error")
    endpoints[1] + diff(endpoints) * 0:nint / nint
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



"[.shingle" <-
    function(x, subset, drop = FALSE)
{
    if (!is.shingle(x)) stop("x must be a shingle")
    ans <- as.numeric(x)[subset]
    attr(ans, "levels") <- levels(x)
    class(attr(ans, "levels")) <- "shingleLevel"
    if (drop) {
        xlvs <- levels(ans)
        dl <- logical(nlevels(ans))
        for (i in seq(along=dl))
            dl[i] <- any( ans >= xlvs[[i]][1] & ans <= xlvs[[i]][2] )
        attr(ans, "levels") <- xlvs[dl]
        class(attr(ans, "levels")) <- "shingleLevel"
    }
    class(ans) <- "shingle"
    ans
}



Rows <- function(x, which)
{
    for (i in seq(along = x)) x[[i]] <-
        rep(x[[i]], length = max(which, length(which)))[which]
    x
}


reorder.factor <- function(Factor, X, Function = mean,
                           ..., 
                           order = is.ordered(Factor))
    (if (order) ordered else factor)(Factor,
                                     levels = names(sort(tapply(X,
                                     Factor, Function, ...))))




make.list.from.intervals <- function(x)
{
    if (ncol(x)!=2) stop("x must be matrix with 2 columns")
    if (nrow(x)<1) stop("x must be matrix with at least 1 row")
    ans <- as.list(1:nrow(x))
    for (i in 1:nrow(x))
        ans[[i]] <- x[i,]
    ans
}



equal.count <-
    function(x, ...)
{
    attr(x, "levels") <- make.list.from.intervals(co.intervals(x,...))
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}



shingle <-
    function(x, intervals=sort(unique(x)))
{
    if (ncol(as.matrix(intervals))==1)
        intervals <- cbind(intervals, intervals)
    else if (ncol(as.matrix(intervals)) > 2)
        stop("bad value of 'intervals'")
    attr(x, "levels") <- make.list.from.intervals(intervals)
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}


as.data.frame.shingle <- as.data.frame.factor

is.shingle <-
    function(x) inherits(x, "shingle")


as.shingle <-
    function(x) if (is.shingle(x)) x else shingle(x)



summary.shingle <- function(object, ...) print.shingle(object, ...)


print.shingleLevel <-
    function(x, ...) {
        print(do.call("rbind", x))
        invisible(x)
    }

print.shingle <- function(x, showValues = TRUE, ...) {
    cat("\nData:\n")
    if (showValues) print(as.numeric(x))
    l <- levels(x)
    n <- nlevels(x)
    if (n<1) cat("\nno intervals\n")
    else {
        int <- data.frame(min = numeric(n), max = numeric(n), count = numeric(n))
        for (i in 1:n) {
            int$min[i] <- l[[i]][1]
            int$max[i] <- l[[i]][2]
            int$count[i] <- length(x[x>=l[[i]][1] & x<=l[[i]][2]])
        }
        cat("\nIntervals:\n")
        print(int)
        olap <- numeric(n-1)
        if (n>2)
            for (i in 1:(n-1))
                olap[i] <- length(x[ x>=l[[i]][1] & x<=l[[i]][2] &
                                    x>=l[[i+1]][1] & x<=l[[i+1]][2]])
        cat("\nOvrlap between adjacent intervals:\n")
        print(olap)
    }
    invisible(x)
}





strip.default <-
    function(which.given,
             which.panel,
             var.name,
             factor.levels,
             shingle.intervals,
             strip.names = c(FALSE, TRUE),
             style = 1,
             ## FIXME: not sure how to incorporate alpha in strip colors
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"))
{
    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length = 2)
    
    if (is.null(factor.levels)) { # means this is a  shingle, as opposed to a factor
        if (is.null(shingle.intervals)) stop("both factor.levels and shingle.intervals cannot be NULL")
        strip.names <- strip.names[2]
        grid.rect(gp = gpar(fill = bg, col = bg))
        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level,])-t[1])/diff(t)
        grid.rect(x = unit(r%*%c(.5,.5),"npc"), width = max(unit( c(diff(r), 1), c("npc", "mm"))),
                  gp = gpar(col=fg, fill=fg))
        if (strip.names)
            grid.text(label = name,
                      gp = 
                      gpar(col = par.strip.text$col,
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
    }
    else if (is.null(shingle.intervals)) { # factor
        strip.names <- strip.names[1]
        x <- factor.levels
        num <- length(x)
        if (style == 1) {
            grid.rect(gp = gpar(fill = bg, col = bg))
            if (strip.names) {
                grid.text(name,
                          x=unit(0.5, "npc") - unit(1, "mm"),
                          gp =
                          gpar(col = par.strip.text$col,
                               alpha = par.strip.text$alpha,
                               fontfamily = par.strip.text$fontfamily,
                               fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                               cex = par.strip.text$cex),
                          just="right")
                grid.text(":",
                          x=unit(0.5, "npc"),
                          gp =
                          gpar(col = par.strip.text$col,
                               alpha = par.strip.text$alpha,
                               fontfamily = par.strip.text$fontfamily,
                               fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                               cex = par.strip.text$cex))
                grid.text(x[level],
                          x=unit(0.5, "npc") + unit(1, "mm"),
                          gp =
                          gpar(col = par.strip.text$col,
                               alpha = par.strip.text$alpha,
                               fontfamily = par.strip.text$fontfamily,
                               fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                               cex = par.strip.text$cex),
                          just="left")
            }
            else grid.text(label = x[level],
                           gp =
                           gpar(col = par.strip.text$col,
                                alpha = par.strip.text$alpha,
                                fontfamily = par.strip.text$fontfamily,
                                fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                                cex = par.strip.text$cex))
        }
        else if (style == 2) {
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill = fg, col = fg))
            grid.text(label=x,
                      x = (2*1:num-1)/(2*num),
                      gp =
                      gpar(col = par.strip.text$col,
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
        else if (style == 3){
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill = fg, col = fg))
            grid.text(label =
                      if (strip.names) paste(name, x[level], sep = ": ")
                      else x[level],
                      gp =
                      gpar(col = par.strip.text$col, 
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
        else if(style == 4){
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill = fg, col = fg))
            grid.text(label=x,
                      x = (2* 1:num - 1)/(2*num),   #using default.units
                      gp =
                      gpar(col = par.strip.text$col, 
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
        else if(style >= 5){
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.text(label=x[level],
                      x = (2* level - 1)/(2*num),   #using default.units
                      gp =
                      gpar(col = par.strip.text$col, 
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
    }
}







lsegments <-
    function(x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL,
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
                  col=col, lwd=lwd, alpha = alpha),
                  default.units="native")
}


larrows <-
    function(x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL,
             angle = 30, code = 2, length = NULL, proportion = .05, ...) 
{

    if (missing(x0)) {x0 <- x1; x1 <- x2}
    if (missing(y0)) {y0 <- y1; y1 <- y2}
    if (!is.null(length)) warning("length not implemented in larrows, use proportion instead")

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

    else if (type == "h") {
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
    else if (type == "h") {
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
    else if (type == "H") {
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


