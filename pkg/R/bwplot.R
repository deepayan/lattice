

### Copyright (C) 2001-2005 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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






prepanel.default.bwplot <-
    function(x, y, box.ratio,
             horizontal = TRUE, nlevels,
             origin = NULL, stack = FALSE,
             ...)
{
    ## This function needs to work for all high level functions in the
    ## bwplot family, namely bwplot, dotplot, stripplot and
    ## barchart. For all but barchart, this is simply a question of
    ## getting the ranges. For stacked barcharts, things are slightly
    ## complicated.

    if (length(x) && length(y))
    {
        #if (!is.numeric(x)) x <- as.numeric(x)
        #if (!is.numeric(y)) y <- as.numeric(y)

        temp <- .5  #* box.ratio/(box.ratio+1)
        if (horizontal)
        {
            if (!is.factor(y)) ## y came from a shingle
            {
                if (missing(nlevels)) nlevels <- length(unique(y))
                y <- factor(y, levels = 1:nlevels)
            }
            list(xlim =
                 if (stack) {
                     foo1 <- if (any(x > 0)) range( by(x[x>0], y[x>0, drop = TRUE], sum)) else 0
                     foo2 <- if (any(x < 0)) range( by(x[x<0], y[x<0, drop = TRUE], sum)) else 0
                     range(foo1, foo2)
                 }
                 else if (is.numeric(x)) range(x[is.finite(x)], origin)
                 else levels(x),
                 ylim = levels(y),
                 yat = sort(unique(as.numeric(y))),
                 dx = 1,
                 dy = 1)
        }
        else
        {
            if (!is.factor(x)) ## x came from a shingle
            {
                if (missing(nlevels)) nlevels <- length(unique(x))
                x <- factor(x, levels = 1:nlevels)
            }
            list(xlim = levels(x),
                 xat = sort(unique(as.numeric(x))),
                 ylim =
                 if (stack) {
                     foo1 <- if (any(y > 0)) range( by(y[y>0], x[y>0], sum)) else 0
                     foo2 <- if (any(y < 0)) range( by(y[y<0], x[y<0], sum)) else 0
                     range(foo1, foo2)
                 }
                 else if (is.numeric(y)) range(y[is.finite(y)], origin)
                 else levels(y),
                 dx = 1,
                 dy = 1)
        }
    }
    else list(xlim = c(NA, NA),
              ylim = c(NA, NA),
              dx = 1, dy = 1)
}





panel.barchart <-
    function(x, y, box.ratio = 1,
             horizontal = TRUE,
             origin = NULL, reference = TRUE,
             stack = FALSE,
             groups = NULL, 
             col = if (is.null(groups)) bar.fill$col else superpose.fill$col,
             border = if (is.null(groups)) bar.fill$border else superpose.fill$border,
             lty = if (is.null(groups)) bar.fill$lty else superpose.fill$lty,
             lwd = if (is.null(groups)) bar.fill$lwd else superpose.fill$lwd,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x) < 1) return()

    bar.fill <- trellis.par.get("bar.fill")
    superpose.fill <- trellis.par.get("superpose.fill")
    reference.line <- trellis.par.get("reference.line")


    ## function defined here so that panel.barchart doesn't need to
    ## have a subscript argument (which would make stripplot always
    ## pass the subscripts to the trellis object, which is unnecessary
    ## when groups = NULL)

    groupSub <- function(groups, subscripts, ...)
        groups[subscripts]

    if (horizontal)
    {

        ## No grouping

        if (is.null(groups))
        {
            if (is.null(origin))
            {
                origin <- current.viewport()$xscale[1]
                reference <- FALSE
            }
            height <- box.ratio/(1+box.ratio)
        
            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)
            grid.rect(gp =
                      gpar(fill = col, col = border,
                           lty = lty, lwd = lwd),
                      y = y,
                      x = rep(origin, length(y)),
                      height = rep(height, length(y)),
                      width = x - origin,
                      just = c("left", "centre"),
                      default.units = "native")
        }

        ## grouped, with stacked bars

        else if (stack)
        {

            if (!is.null(origin) && origin != 0)
                warning("origin forced to 0 for stacked bars")

            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            col <- rep(col, length = nvals)
            border <- rep(border, length = nvals)
            lty <- rep(lty, length = nvals)
            lwd <- rep(lwd, length = nvals)

            height <- box.ratio/(1 + box.ratio)

            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)

            for (i in unique(y))
            {
                ok <- y == i
                ord <- sort.list(groups[ok])
                pos <- x[ok][ord] > 0
                nok <- sum(pos)
                if (nok > 0)
                    grid.rect(gp =
                              gpar(fill = col[groups[ok][ord][pos]],
                                   col = border[groups[ok][ord][pos]],
                                   lty = lty[groups[ok][ord][pos]],
                                   lwd = lwd[groups[ok][ord][pos]]),
                              y = rep(i, nok),
                              x = cumsum(c(0, x[ok][ord][pos][-nok])),
                              height = rep(height, nok),
                              width = x[ok][ord][pos],
                              just = c("left", "centre"),
                              default.units = "native")
                neg <- x[ok][ord] < 0
                nok <- sum(neg)
                if (nok > 0)
                    grid.rect(gp =
                              gpar(fill = col[groups[ok][ord][neg]],
                                   col = border[groups[ok][ord][neg]],
                                   lty = lty[groups[ok][ord][neg]],
                                   lwd = lwd[groups[ok][ord][neg]]),
                              y = rep(i, nok),
                              x = cumsum(c(0, x[ok][ord][neg][-nok])),
                              height = rep(height, nok),
                              width = x[ok][ord][neg],
                              just = c("left", "centre"),
                              default.units = "native")
            }
        }

        ## grouped, with side by side bars

        else
        {
            if (is.null(origin))
            {
                origin <- current.viewport()$xscale[1]
                reference <- FALSE
            }
            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            col <- rep(col, length = nvals)
            border <- rep(border, length = nvals)
            lty <- rep(lty, length = nvals)
            lwd <- rep(lwd, length = nvals)

            height <- box.ratio/(1 + nvals * box.ratio)
            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)
            for (i in unique(y))
            {
                ok <- y == i
                nok <- sum(ok)
                grid.rect(gp =
                          gpar(fill = col[groups[ok]],
                               col = border[groups[ok]],
                               lty = lty[groups[ok]],
                               lwd = lwd[groups[ok]]),
                          y = (i + height * (groups[ok] - (nvals + 1)/2)),
                          x = rep(origin, nok), 
                          height = rep(height, nok),
                          width = x[ok] - origin,
                          just = c("left", "centre"),
                          default.units = "native")
            }
        }
    }
    
    ## if not horizontal

    else
    {
        if (is.null(groups))
        {
            if (is.null(origin))
            {
                origin <- current.viewport()$yscale[1]
                reference <- FALSE
            }
            width <- box.ratio/(1+box.ratio)
        
            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)

            grid.rect(gp =
                      gpar(fill = col, col = border,
                           lty = lty, lwd = lwd),
                      x = x,
                      y = rep(origin, length(x)),
                      width = rep(width, length(x)),
                      height = y - origin,
                      just = c("centre", "bottom"),
                      default.units = "native")
        }
        else if (stack)
        {

            if (!is.null(origin) && origin != 0)
                warning("origin forced to 0 for stacked bars")

            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            col <- rep(col, length = nvals)
            border <- rep(border, length = nvals)
            lty <- rep(lty, length = nvals)
            lwd <- rep(lwd, length = nvals)

            width <- box.ratio/(1 + box.ratio)

            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)

            for (i in unique(x))
            {
                ok <- x == i
                ord <- sort.list(groups[ok])
                pos <- y[ok][ord] > 0
                nok <- sum(pos)
                if (nok > 0)
                    grid.rect(gp =
                              gpar(fill = col[groups[ok][ord][pos]],
                                   col = border[groups[ok][ord][pos]],
                                   lty = lty[groups[ok][ord][pos]],
                                   lwd = lwd[groups[ok][ord][pos]]),
                              x = rep(i, nok),
                              y = cumsum(c(0, y[ok][ord][pos][-nok])),
                              width = rep(width, nok),
                              height = y[ok][ord][pos],
                              just = c("centre", "bottom"),
                              default.units = "native")
                neg <- y[ok][ord] < 0
                nok <- sum(neg)
                if (nok > 0)
                    grid.rect(gp =
                              gpar(fill = col[groups[ok][ord][neg]],
                                   col = border[groups[ok][ord][neg]],
                                   lty = lty[groups[ok][ord][neg]],
                                   lwd = lwd[groups[ok][ord][neg]]),
                              x = rep(i, nok),
                              y = cumsum(c(0, y[ok][ord][neg][-nok])),
                              width = rep(width, nok),
                              height = y[ok][ord][neg],
                              just = c("centre", "bottom"),
                              default.units = "native")
            }

            
        }
        else
        {
            if (is.null(origin))
            {
                origin <- current.viewport()$yscale[1]
                reference = FALSE
            }
            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            col <- rep(col, length = nvals)
            border <- rep(border, length = nvals)
            lty <- rep(lty, length = nvals)
            lwd <- rep(lwd, length = nvals)

            width <- box.ratio/(1 + nvals * box.ratio)
            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)
            for (i in unique(x))
            {
                ok <- x == i
                nok <- sum(ok)
                grid.rect(gp =
                          gpar(fill = col[groups[ok]],
                               col = border[groups[ok]],
                               lty = lty[groups[ok]],
                               lwd = lwd[groups[ok]]),
                          x = (i + width * (groups[ok] - (nvals + 1)/2)),
                          y = rep(origin, nok), 
                          width = rep(width, nok),
                          height = y[ok] - origin,
                          just = c("centre", "bottom"),
                          default.units = "native")
            }
        }
    }
}



panel.dotplot <-
    function(x, y, horizontal = TRUE,
             pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch,
             col = if (is.null(groups)) dot.symbol$col else sup.symbol$col,
             lty = dot.line$lty,
             lwd = dot.line$lwd,
             col.line = dot.line$col,
             levels.fos = if (horizontal) unique(y) else unique(x),
             groups = NULL,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")

    if (horizontal)
    {
        yscale <- current.viewport()$yscale
        panel.abline(h = levels.fos,
                     col = col.line,
                     lty = lty, lwd = lwd)
        if (is.null(groups)) 
            panel.xyplot(x = x, y = y, col = col,
                         pch = pch, lty = lty, lwd = lwd, 
                         horizontal = horizontal, ...)
        else
            panel.superpose(x = x, y = y, groups = groups,
                            col = col, pch = pch,
                            lty = lty, lwd = lwd, 
                            horizontal = horizontal, ...)
    }
    else
    {
        xscale <- current.viewport()$xscale
        panel.abline(v = levels.fos, col = col.line,
                     lty = lty, lwd = lwd)
        if (is.null(groups)) 
            panel.xyplot(x = x, y = y, col = col, pch = pch,
                         lty = lty, lwd = lwd, 
                         horizontal = horizontal, ...)
        else 
            panel.superpose(x = x, y = y, groups = groups,
                            col = col, pch = pch,
                            lty = lty, lwd = lwd, 
                            horizontal = horizontal, ...)
    }
}





panel.stripplot <-
    function(x, y, jitter.data = FALSE, factor = 0.5,
             horizontal = TRUE, groups = NULL, ...)
{
    if (length(x) < 1) return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    y.jitter  <-
        if (horizontal && jitter.data) jitter(y, factor = factor)
        else y
    x.jitter  <-
        if (!horizontal && jitter.data) jitter(x, factor = factor)
        else x
    if (is.null(groups)) panel.xyplot(x = x.jitter, y = y.jitter,
                                      horizontal = horizontal, ...)
    else panel.superpose(x = x.jitter, y = y.jitter, groups = groups,
                         horizontal = horizontal, ...)
}




panel.bwplot <-
    function(x, y, box.ratio = 1,
             horizontal = TRUE,
             pch = box.dot$pch,
             col = box.dot$col,
             cex = box.dot$cex,
             font = box.dot$font,
             fontfamily = box.dot$fontfamily,
             fontface = box.dot$fontface, 
             fill = box.rectangle$fill,
             varwidth = FALSE,
             levels.fos = if (horizontal) unique(y) else unique(x),
             coef = 1.5, do.out = TRUE, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x) < 1) return()

    box.dot <- trellis.par.get("box.dot")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")

    fontsize.points <- trellis.par.get("fontsize")$points
    xscale <- current.viewport()$xscale
    yscale <- current.viewport()$yscale

    if (horizontal)
    {

        maxn <- max(by(x, y, length)) ## used if varwidth = TRUE
        ##lower <- ceiling(yscale[1])
        height <- box.ratio/(1+box.ratio)

        for (yval in levels.fos)
        {

            ## yval  <- i
            stats <- boxplot.stats(x[y==yval], coef = coef, do.out = do.out)
            
            if (stats$n>0)
            {
                pushViewport(viewport(y=unit(yval, "native"),
                                      height = unit((if (varwidth)
                                      sqrt(stats$n/maxn)  else 1) * height, "native"),
                                      xscale = xscale))
                
                r.x <- (stats$stats[2]+stats$stats[4])/2
                r.w <- stats$stats[4]-stats$stats[2]
                grid.rect(x = unit(r.x, "native"), width = unit(r.w, "native"),
                          gp = gpar(lwd = box.rectangle$lwd,
                          lty = box.rectangle$lty,
                          fill = fill,
                          col = box.rectangle$col))
                
                grid.lines(x = unit(stats$stats[1:2],"native"),
                           y=unit(c(.5,.5), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.lines(x = unit(stats$stats[4:5],"native"),
                           y=unit(c(.5,.5), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.lines(x = unit(rep(stats$stats[1],2),"native"),
                           y=unit(c(0,1), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.lines(x = unit(rep(stats$stats[5],2),"native"),
                           y=unit(c(0,1), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.points(x = stats$stats[3], y = .5, pch = pch, 
                            gp =
                            gpar(col = col, cex = cex,
                                 fontfamily = fontfamily,
                                 fontface = chooseFace(fontface, font),
                                 fontsize = fontsize.points))
                
                if ((l<-length(stats$out))>0)
                    grid.points(x = stats$out, y = rep(.5,l),
                                pch = plot.symbol$pch,
                                gp =
                                gpar(col = plot.symbol$col,
                                     cex = plot.symbol$cex,
                                     fontfamily = plot.symbol$fontfamily,
                                     fontface = chooseFace(plot.symbol$fontface, plot.symbol$font),
                                     fontsize = fontsize.points))
                
                popViewport()
                
            }
        }
        
    }
    else
    {

        maxn <- max(by(y, x, length)) ## used if varwidth = TRUE
        ##lower <- ceiling(xscale[1])
        width <- box.ratio/(1+box.ratio)

        for (xval in levels.fos)
        {
            ##xval  <- i
            stats <- boxplot.stats(y[x==xval], coef = coef, do.out = do.out)

            if (stats$n>0)
            {
                pushViewport(viewport(x = unit(xval, "native"),
                                      width = unit((if (varwidth)
                                      sqrt(stats$n/maxn)  else 1) * width, "native"),
                                      yscale = yscale))
                
                r.x <- (stats$stats[2]+stats$stats[4])/2
                r.w <- stats$stats[4]-stats$stats[2]
                grid.rect(y = unit(r.x, "native"), height = unit(r.w, "native"),
                          gp = gpar(lwd = box.rectangle$lwd,
                          lty = box.rectangle$lty,
                          fill = fill,
                          col = box.rectangle$col))
                
                grid.lines(y = unit(stats$stats[1:2],"native"),
                           x = unit(c(.5,.5), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.lines(y = unit(stats$stats[4:5],"native"),
                           x = unit(c(.5,.5), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.lines(y = unit(rep(stats$stats[1],2),"native"),
                           x = unit(c(0,1), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.lines(y = unit(rep(stats$stats[5],2),"native"),
                           x = unit(c(0,1), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lwd = box.umbrella$lwd,
                           lty = box.umbrella$lty))
                
                grid.points(y = stats$stats[3], x = .5, pch = pch, 
                            gp =
                            gpar(col = col, cex = cex,
                                 fontfamily = fontfamily,
                                 fontface = chooseFace(fontface, font),
                                 fontsize = fontsize.points))
                
                if ((l<-length(stats$out))>0)
                    grid.points(y = stats$out, x = rep(.5,l),
                                pch = plot.symbol$pch,
                                gp =
                                gpar(col = plot.symbol$col,
                                     cex = plot.symbol$cex,
                                     fontfamily = plot.symbol$fontfamily,
                                     fontface = chooseFace(plot.symbol$fontface, plot.symbol$font),
                                     fontsize = fontsize.points))
                
                popViewport()
                
            }
        }
        
    }
}

















panel.violin <-
    function(x, y, box.ratio = 1, horizontal = TRUE,

             alpha = bar.fill$alpha,
             border = bar.fill$border,
             lty = bar.fill$lty,
             lwd = bar.fill$lwd,
             col = bar.fill$col,

             varwidth = FALSE,

             bw = NULL,
             adjust = NULL,
             kernel = NULL,
             window = NULL,
             width = NULL,
             n = 50,
             from = NULL,
             to = NULL,
             cut = NULL,
             na.rm = TRUE,
             
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x) < 1) return()

    ##reference.line <- trellis.par.get("reference.line")
    bar.fill <- trellis.par.get("bar.fill")

    ## density doesn't handle unrecognized arguments (not even to
    ## ignore it).  A tedious but effective way to handle that is to
    ## have all arguments to density be formal arguments to this panel
    ## function, as follows:

    darg <- list()
    darg$bw <- bw
    darg$adjust <- adjust
    darg$kernel <- kernel
    darg$window <- window
    darg$width <- width
    darg$n <- n
    darg$from <- from
    darg$to <- to
    darg$cut <- cut
    darg$na.rm <- na.rm

    my.density <- function(x) do.call("density", c(list(x = x), darg))

    numeric.list <- if (horizontal) split(x, factor(y)) else split(y, factor(x))
    levels.fos <- as.numeric(names(numeric.list))
    d.list <- lapply(numeric.list, my.density)
    ## n.list <- sapply(numeric.list, length)  UNNECESSARY
    dx.list <- lapply(d.list, "[[", "x")
    dy.list <- lapply(d.list, "[[", "y")

    max.d <- sapply(dy.list, max)
    if (varwidth) max.d[] <- max(max.d)

    ##str(max.d)
    
    xscale <- current.viewport()$xscale
    yscale <- current.viewport()$yscale
    height <- box.ratio / (1 + box.ratio)

    if (horizontal)
    {
        for (i in seq(along = levels.fos))
        {
            pushViewport(viewport(y = unit(levels.fos[i], "native"),
                                  height = unit(height, "native"),
                                  yscale = c(max.d[i] * c(-1, 1)),
                                  xscale = xscale))
            grid.polygon(x = c(dx.list[[i]], rev(dx.list[[i]])),
                         y = c(dy.list[[i]], -rev(dy.list[[i]])),
                         default.units = "native",
                         gp = gpar(fill = col, col = border, lty = lty, lwd = lwd, alpha = alpha))
            popViewport()
        }
    }
    else
    {
        for (i in seq(along = levels.fos))
        {
            pushViewport(viewport(x = unit(levels.fos[i], "native"),
                                  width = unit(height, "native"),
                                  xscale = c(max.d[i] * c(-1, 1)),
                                  yscale = yscale))
            grid.polygon(y = c(dx.list[[i]], rev(dx.list[[i]])),
                         x = c(dy.list[[i]], -rev(dy.list[[i]])),
                         default.units = "native",
                         gp = gpar(fill = col, col = border, lty = lty, lwd = lwd, alpha = alpha))
            popViewport()
        }
    }
    invisible()
}























dotplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.dotplot",
             groups = NULL,
             ...,
             subset = TRUE)
{

    ## m <- match.call(expand.dots = FALSE)
    ## lapply(dots, eval, data, parent.frame())))

    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    right.name <- deparse(substitute(formula))

    try(formula <- eval(formula), silent = TRUE)

    foo <- substitute(formula)

    if (!(is.call(foo) && foo[[1]] == "~")) {
        formula <- as.formula(paste("~", right.name)) #   deparse(foo)))
        environment(formula) <- parent.frame()
    }

    call.list <- c(list(formula = formula, data = data,
                        groups = groups,
                        subset = subset,
                        panel = panel,
                        box.ratio = 0),
                   dots)
    ans <- do.call("bwplot", call.list)
    ans$call <- match.call()
    ans
}



barchart <-
    function(formula,
             data = parent.frame(),
             panel = "panel.barchart",
             box.ratio = 2,
             groups = NULL,
             ...,
             subset = TRUE)
{

    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    right.name <- deparse(substitute(formula))

    try(formula <- eval(formula), silent = TRUE)

    foo <- substitute(formula)

    if (!(is.call(foo) && foo[[1]] == "~")) {
        formula <- as.formula(paste("~", right.name)) #   deparse(foo)))
        environment(formula) <- parent.frame()
    }

    call.list <- c(list(formula = formula, data = data,
                        groups = groups,
                        subset = subset,
                        panel = panel,
                        box.ratio = box.ratio),
                   dots)
    ans <- do.call("bwplot", call.list)
    ans$call <- match.call()
    ans
}


stripplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.stripplot",
             jitter = FALSE,
             factor = .5,
             box.ratio = if (jitter) 1 else 0,
             groups = NULL,
             ...,
             subset = TRUE)
{

    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    right.name <- deparse(substitute(formula))

    try(formula <- eval(formula), silent = TRUE)

    foo <- substitute(formula)

    if (!(is.call(foo) && foo[[1]] == "~")) {
        formula <- as.formula(paste("~", right.name)) #   deparse(foo)))
        environment(formula) <- parent.frame()
    }

    call.list <- c(list(formula = formula, data = data,
                        panel = panel,
                        jitter = jitter,
                        factor = factor,
                        groups = groups,
                        subset = subset,
                        box.ratio = box.ratio),
                   dots)

    ans <- do.call("bwplot", call.list)
    ans$call <- match.call()
    ans
}


bwplot <-
    function(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = "fill",
             panel = "panel.bwplot",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             box.ratio = 1,
             horizontal = NULL,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales =
             if (horizontal) list(y = list(tck = 0, alternating = FALSE, rot = 0))
             else list(x = list(tck = 0, alternating = FALSE)),
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ##m <- match.call(expand.dots = FALSE)
    ##dots <- m$...
    ##dots <- lapply(dots, eval, data, parent.frame())

    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

    formname <- deparse(substitute(formula))
    formula <- eval(substitute(formula), data, parent.frame())

    if (!inherits(formula, "formula"))
        formula <- as.formula(paste("~", formname))
    
    form <-
        latticeParseFormula(formula, data, subset = subset,
                            groups = groups, multiple = allow.multiple,
                            outer = outer, subscripts = TRUE,
                            drop = drop.unused.levels)


    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if (subscripts) subscr <- form$subscr

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    cond <- form$condition
    number.of.cond <- length(cond)
    x <- form$right
    y <- form$left
    if (is.null(y))
    {
        y <- rep(if (is.null(names(x))) '' else names(x), length = length(x))
        y <- factor(y, levels = unique(y))
    }
    if (number.of.cond == 0)
    {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        number.of.cond <- 1
    }


    if (is.null(horizontal)) {
        horizontal <-
            if ((is.factor(x) || is.shingle(x)) && is.numeric(y)) FALSE
            else TRUE
    }


    if (horizontal)
    {
        if (!(is.numeric(x)))
        {
            warning("x should be numeric")
        }
        y <- as.factorOrShingle(y)
        is.f.y <- is.factor(y)  # used throughout the rest of the code
        num.l.y <- nlevels(y)

        if (missing(xlab)) xlab <- form$right.name
        if (missing(ylab)) ylab <- if (is.f.y) NULL else form$left.name
    }
    else
    {
        if (!(is.numeric(y)))
        {
            warning("y should be numeric")
        }
        x <- as.factorOrShingle(x)
        is.f.x <- is.factor(x)  # used throughout the rest of the code
        num.l.x <- nlevels(x)

        if (missing(ylab)) ylab <- form$left.name
        if (missing(xlab)) xlab <- if (is.f.x) NULL else form$right.name
    }

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(cond = cond,
                          aspect = aspect,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab,
                          xlab.default = form$right.name,
                          ylab.default = form$left.name), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    ##scales <- 
    ##if (missing(scales)) scales 
    ##else eval(m$scales, data, parent.frame())


    ## The following is to make the default alternating FALSE for factors
    if (is.character(scales)) scales <- list(relation = scales)
#     if (is.null(scales$alternating)) {
#         if (horizontal) {
#             if (is.null(scales$y)) scales$y <- list(alternating = FALSE)
#             else if (is.null(scales$y$alternating)) scales$y$alternating <- FALSE
#         ## bug if y="free" ? but who cares
#         }
#         else {
#             if (is.null(scales$x)) scales$x <- list(alternating = FALSE)
#             else if (is.null(scales$x$alternating)) scales$x$alternating <- FALSE
#         ## bug if x="free" ? but who cares
#         }
#     }
    scales <- updateList(default.scales, scales)
    foo <- c(foo,
             do.call("construct.scales", scales))

    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit)) {
        have.ylim <- TRUE
        ylim <- foo$y.scales$limit
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (have.xlim) xlim <- log(xlim, xbase)
    }
    if (have.ylog) {
        ## warning("Are you sure you want log scale for y ?")
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (have.ylim) ylim <- log(ylim, ybase)
    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- is.na(x)|is.na(y)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <- dots
    foo$panel.args.common$box.ratio <- box.ratio
    foo$panel.args.common$horizontal <- horizontal
    if (subscripts) foo$panel.args.common$groups <- groups

    ## only used if shingle, important if some levels are missing
    if (horizontal)
    {
        if (!is.f.y) ## y shingle
            foo$panel.args.common$nlevels <- num.l.y
    }
    else
    {
        if (!is.f.x) ## x shingle
            foo$panel.args.common$nlevels <- num.l.x
    }

    nplots <- prod(cond.max.level)
    if (nplots != prod(sapply(foo$condlevels, length))) stop("mismatch")
    foo$panel.args <- vector(mode = "list", length = nplots)


    cond.current.level <- rep(1, number.of.cond)


    for (panel.number in seq(length = nplots))
    {

        id <- !id.na
        for(i in 1:number.of.cond)
        {
            var <- cond[[i]]
            id <- id &
            if (is.shingle(var))
                ((var >=
                  levels(var)[[cond.current.level[i]]][1])
                 & (var <=
                    levels(var)[[cond.current.level[i]]][2]))
            else (as.numeric(var) == cond.current.level[i])
        }

        if (horizontal)
        {
            if (is.f.y)
            {
                foo$panel.args[[panel.number]] <-
                    list(x = x[id],
                         ##y = as.numeric(y[id]))
                         y = y[id])
                if (subscripts)
                    foo$panel.args[[panel.number]]$subscripts <-
                        subscr[id]
            }
            else  # shingle
            {
                panel.x <- numeric(0)
                panel.y <- numeric(0)
                if (subscripts) panel.subscr <- numeric(0)
                for (k in seq(length = num.l.y))
                {
                    tid <- id & (y >= levels(y)[[k]][1]) & (y <= levels(y)[[k]][2])
                    panel.x <- c(panel.x, x[tid])
                    panel.y <- c(panel.y, rep(k,length(tid[tid])))
                    if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                }
                foo$panel.args[[panel.number]] <-
                    list(x = panel.x,
                         y = panel.y)
                if (subscripts)
                    foo$panel.args[[panel.number]]$subscripts <-
                        panel.subscr

            }
        }
        else
        {
            if (is.f.x)
            {
                foo$panel.args[[panel.number]] <-
                    ##list(x = as.numeric(x[id]),
                    list(x = x[id],
                         y = y[id])
                if (subscripts)
                    foo$panel.args[[panel.number]]$subscripts <-
                        subscr[id]
            }
            else   # shingle
            {
                panel.x <- numeric(0)
                panel.y <- numeric(0)
                if (subscripts) panel.subscr <- numeric(0)
                for (k in seq(length = num.l.x))
                {
                    tid <- id & (x >= levels(x)[[k]][1]) & (x <= levels(x)[[k]][2])
                    panel.y <- c(panel.y, y[tid])
                    panel.x <- c(panel.x, rep(k,length(tid[tid])))
                    if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                }
                foo$panel.args[[panel.number]] <-
                    list(x = panel.x,
                         y = panel.y)
                if (subscripts)
                    foo$panel.args[[panel.number]]$subscripts <-
                        panel.subscr
            }
        }

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }



    more.comp <- c(limits.and.aspect(prepanel.default.bwplot,
                                     prepanel = prepanel, 
                                     have.xlim = have.xlim, xlim = xlim, 
                                     have.ylim = have.ylim, ylim = ylim, 
                                     x.relation = foo$x.scales$relation,
                                     y.relation = foo$y.scales$relation,
                                     panel.args.common = foo$panel.args.common,
                                     panel.args = foo$panel.args,
                                     aspect = aspect,
                                     nplots = nplots,
                                     x.axs = foo$x.scales$axs,
                                     y.axs = foo$y.scales$axs),
                   cond.orders(foo))
    foo[names(more.comp)] <- more.comp


    if (is.null(foo$legend) && !is.null(groups) &&
        (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                      args =
                      updateList(list(text = levels(as.factor(groups)),
                                      points = TRUE,
                                      rectangles = FALSE,
                                      lines = FALSE), 
                                 if (is.list(auto.key)) auto.key else list())))
        foo$legend[[1]]$x <- foo$legend[[1]]$args$x
        foo$legend[[1]]$y <- foo$legend[[1]]$args$y
        foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

        names(foo$legend) <- 
            if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
                "inside"
            else
                "top"
        if (!is.null(foo$legend[[1]]$args$space))
            names(foo$legend) <- foo$legend[[1]]$args$space
    }

    class(foo) <- "trellis"
    foo
}

