

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




prepanel.default.xyplot <-
    function(x, y, type, subscripts, groups, ...)
{

    ## Note: shingles satisfy is.numeric()
    if (any(!is.na(x)) && any(!is.na(y))) {

        if (!missing(groups))
        {
            vals <-
                if (is.factor(groups)) levels(groups)
                else sort(unique(groups))

            dx <- numeric(0)
            dy <- numeric(0)
            for (i in seq(along = vals))
            {
                id <- (groups[subscripts] == vals[i])
                ord <- order(x[id])
                dx <- c(dx, as.numeric(diff(x[id][ord])))
                dy <- c(dy, as.numeric(diff(y[id][ord])))
            }
        }
        else
        {
            ord <- order(x)
            dx = as.numeric(diff(x[ord]))
            dy = as.numeric(diff(y[ord]))            
        }
        list(xlim = if (is.numeric(x)) range(x[is.finite(x)]) else levels(x),
             ylim = if (is.numeric(y)) range(y[is.finite(y)]) else levels(y),
             dx = dx, dy = dy)

    }
    else list(xlim = c(NA, NA),
              ylim = c(NA, NA),
              dx = NA, dy = NA)
}




panel.xyplot <-
    function(x, y, type = "p",
             pch = plot.symbol$pch,
             col,
             col.line = plot.line$col,
             col.symbol = plot.symbol$col,
             font = plot.symbol$font,
             fontfamily = plot.symbol$fontfamily,
             fontface = plot.symbol$fontface,
             lty = plot.line$lty,
             cex = plot.symbol$cex,
             lwd = plot.line$lwd,
             horizontal = FALSE, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x) < 1) return()


    if (!missing(col)) {
        if (missing(col.line)) col.line <- col
        if (missing(col.symbol)) col.symbol <- col
    }

    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")

    if ("o" %in% type || "b" %in% type)
        type <- c(type, "p", "l")


    if ("g" %in% type)
        panel.grid(h = -1, v = -1)

    if ("p" %in% type)
        lpoints(x = x, y = y, cex = cex, font = font,
                fontfamily = fontfamily, fontface = fontface,
                col = col.symbol, pch=pch)


    if ("l" %in% type)
        llines(x=x, y=y, lty=lty, col=col.line, lwd=lwd)


    if ("h" %in% type)
        if (horizontal)
            llines(x=x, y=y, type = "H",
                   lty=lty, col=col.line, lwd=lwd)
        else
            llines(x=x, y=y, type = "h",
                   lty=lty, col=col.line, lwd=lwd)


    ## should this be delegated to llines with type='s'?
    if ("s" %in% type) {
        ord <- if (horizontal) sort.list(y) else sort.list(x)
        n <- length(x)
        xx <- numeric(2*n-1)
        yy <- numeric(2*n-1)

        xx[2*1:n-1] <- x[ord]
        yy[2*1:n-1] <- y[ord]
        xx[2*1:(n-1)] <- x[ord][-1]
        yy[2*1:(n-1)] <- y[ord][-n]
        llines(x=xx, y=yy,
               lty=lty, col=col.line, lwd=lwd)
    }
    if ("S" %in% type) {
        ord <- if (horizontal) sort.list(y) else sort.list(x)
        n <- length(x)
        xx <- numeric(2*n-1)
        yy <- numeric(2*n-1)

        xx[2*1:n-1] <- x[ord]
        yy[2*1:n-1] <- y[ord]
        xx[2*1:(n-1)] <- x[ord][-n]
        yy[2*1:(n-1)] <- y[ord][-1]
        llines(x=xx, y=yy,
               lty=lty, col=col.line, lwd=lwd)
    }
    if ("r" %in% type) {
        panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, ...)
    }
    if ("smooth" %in% type) {
        panel.loess(x, y, col = col.line, lty = lty, lwd = lwd, ...)
    }
    
}





xyplot <-
    function(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = "fill",
## FIXME            layout = NULL,
             panel = if (is.null(groups)) "panel.xyplot"
             else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(),
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ##dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

    ## FIXME: make sure this is done everywhere else
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
    y <- form$left
    x <- form$right

    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        ##layout <- c(1,1,1)                           FIXME : changed
        number.of.cond <- 1
    }

    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- form$left.name

    ## S-PLUS requires both x and y to be numeric, but we
    ## don't. Question is, should we give a warning ? Nope.

    ##if (!(is.numeric(x) && is.numeric(y)))
    ##    warning("x and y are not both numeric")


    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(cond = cond,     # FIXME: changed
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

    ##scales <- eval(substitute(scales), data, parent.frame())
    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, 
             do.call("construct.scales", scales))


    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit)) { # override xlim
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


    ## old NA-handling

#     id.na <- is.na(x)|is.na(y)
#     for (var in cond)
#         id.na <- id.na | is.na(var)
#     if (!any(!id.na)) stop("nothing to draw")
#     ## Nothing simpler ?


    
    ## new NA-handling: will retain NA's in x, y

    id.na <- do.call("pmax", lapply(cond, is.na))
    if (!any(!id.na)) stop("nothing to draw")




    
## FIXME:     foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <- dots
    if (subscripts) foo$panel.args.common$groups <- groups

    nplots <- prod(cond.max.level)
    if (nplots != prod(sapply(foo$condlevels, length))) stop("mismatch")
    foo$panel.args <- vector(mode = "list", length = nplots)


    cond.current.level <- rep(1, number.of.cond)


    for (panel.number in seq(length = nplots))
    {

        id <- !id.na
        for (i in 1:number.of.cond)
        {
            var <- cond[[i]]
            id <- id &
            if (is.shingle(var))
                ((var >= levels(var)[[cond.current.level[i]]][1])
                 & (var <= levels(var)[[cond.current.level[i]]][2]))
            else (as.numeric(var) == cond.current.level[i])
        }
        foo$panel.args[[panel.number]] <-
            list(x = x[id], y = y[id])
        if (subscripts)
            foo$panel.args[[panel.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }


    ## FIXME: make this adjustment everywhere else

    more.comp <- c(limits.and.aspect(prepanel.default.xyplot,
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








