

### Copyright 2001  Deepayan Sarkar <deepayan@stat.wisc.edu>
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









prepanel.default.densityplot <-
    function(x,
             darg,
             groups = NULL,
             subscripts = TRUE,
             ...)
{
    if (!is.numeric(x)) x <- as.numeric(x)

    if (length(x) < 1)
        list(xlim = NA,
             ylim = NA,
             dx = NA,
             dy = NA)
    else if (is.null(groups))
    {
        if (length(x) > 1)
        {
            h <- do.call("density", c(list(x=x), darg))
            list(xlim = range(h$x),
                 ylim = range(h$y),
                 dx = diff(h$x), dy = diff(h$y))
        }
        else
            list(xlim = range(x),
                 ylim = 0,
                 dx = 1, dy = 1)
    }
    else
    {
        vals <- sort(unique(groups))
        nvals <- length(vals)
        xl <- range(x)
        yl <- 0
        dxl <- numeric(0) # bad names !!
        dyl <- numeric(0) 
        for (i in seq(along=vals)) {
            id <- (groups[subscripts] == vals[i])
            if (sum(id) > 1)
            {
                h <- do.call("density", c(list(x=x[id]), darg))
                xl <- c(xl, h$x)
                yl <- c(yl, h$y)
                dxl <- c(dxl, diff(h$x))
                dyl <- c(dyl, diff(h$y))
            }
        }
        list(xlim = range(xl), ylim = range(yl), dx = dxl, dy = dyl)
    }
}




panel.densityplot <-
    function(x,
             darg = list(n = 30),
             plot.points = TRUE,
             ref = FALSE,
             col = plot.line$col,
             col.line = col,
             ...)
{
    x <- as.numeric(x)

    if (ref)
    {
        reference.line <- trellis.par.get("reference.line")
        panel.abline(h=0,
                     col = reference.line$col,
                     lty = reference.line$lty,
                     lwd = reference.line$lwd)
    }

    plot.line <- trellis.par.get("plot.line")
    if (length(x) > 1)
    {
        h <- do.call("density", c(list(x=x), darg))
        lim <- current.viewport()$xscale
        id <- (h$x>=lim[1] & h$x<=lim[2])
        llines(x = h$x[id], y = h$y[id], col = col.line, ...)
    }
    if (plot.points) panel.xyplot(x = x, y = rep(0, length(x)), col = col, ...) 
}





densityplot <-
    function(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = "fill",
             panel = if (is.null(groups)) "panel.densityplot" else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             bw = NULL,
             adjust = NULL,
             kernel = NULL,
             window = NULL,
             width = NULL,
             give.Rkern = FALSE,
             n = 50,
             from = NULL,
             to = NULL,
             cut = NULL,
             na.rm = NULL,
             drop.unused.levels = TRUE,
             ...,
             panel.groups = "panel.densityplot",
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    ## darg is a list that gives arguments to density()
    darg <- list()
    darg$bw <- bw
    darg$adjust <- adjust
    darg$kernel <- kernel
    darg$window <- window
    darg$width <- width
    darg$give.Rkern <- give.Rkern
    darg$n <- n
    darg$from <- from
    darg$to <- to
    darg$cut <- cut
    darg$na.rm <- na.rm
    
    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

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
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        number.of.cond <- 1
    }

    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- "Density"

    ##if (!is.numeric(x))
    ##    warning("x should be numeric")
    ##x <- as.numeric(x)

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
                          ylab.default = "Density"), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## scales <- eval(substitute(scales), data, parent.frame())
    if (is.character(scales)) scales <- list(relation = scales)
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
        warning("Can't have log Y-scale")
        have.ylog <- FALSE
        foo$y.scales$log <- FALSE
    }

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- is.na(x)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <- c(dots, list(darg = darg))
    if (subscripts) {
        foo$panel.args.common$groups <- groups
        foo$panel.args.common$panel.groups <- panel.groups
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

        foo$panel.args[[panel.number]] <-
            list(x = x[id])
        if (subscripts)
            foo$panel.args[[panel.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.densityplot,
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
                               y.axs = foo$y.scales$axs))


    if (is.null(foo$legend) && !is.null(groups) &&
        (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                      args =
                      c(list(levels(as.factor(groups))),
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
