

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
    if (sum(!is.na(x)) < 1)
        list(xlim = NA,
             ylim = NA,
             dx = NA,
             dy = NA)
    else if (sum(!is.na(x)) == 1)
    {
        list(xlim = x,
             ylim = 0,
             dx = 1,
             dy = 1)
    }
    else if (is.null(groups))
    {
        h <- do.call(density, c(list(x = x), darg))
        ## for banking calculations, include only middle 70% values
        quants <-
            quantile(x, prob = c(0.15, 0.85),
                     names = FALSE, na.rm = TRUE)
        ok <- h$x > quants[1] & h$x < quants[2]
        list(xlim = range(h$x),
             ylim = range(h$y),
             dx = diff(h$x[ok]),
             dy = diff(h$y[ok]))
    }
    else
    {
        vals <- sort(unique(groups))
        nvals <- length(vals)
        xl <- range(x, finite = TRUE)
        yl <- 0
        dxl <- numeric(0) # bad names !!
        dyl <- numeric(0) 
        for (i in seq(along=vals))
        {
            id <- (groups[subscripts] == vals[i])
            if (sum(id) > 1)
            {
                h <- do.call(density, c(list(x = x[id]), darg))
                xl <- c(xl, h$x)
                yl <- c(yl, h$y)
                ## for banking calculations, include only middle 70% values
                quants <-
                    quantile(x[id], prob = c(0.15, 0.85),
                             names = FALSE, na.rm = TRUE)
                ok <- h$x > quants[1] & h$x < quants[2]
                dxl <- c(dxl, diff(h$x[ok]))
                dyl <- c(dyl, diff(h$y[ok]))
            }
        }
        list(xlim = range(xl, finite = TRUE),
             ylim = range(yl, finite = TRUE),
             dx = dxl, dy = dyl)
    }
}




panel.densityplot <-
    function(x,
             darg = list(n = 30),
             plot.points = "jitter",
             ref = FALSE,
             groups = NULL,
##              col = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
##              lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
##              lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
##              alpha = if (is.null(groups)) plot.line$alpha else superpose.line$alpha,
##              col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
             jitter.amount = 0.01 * diff(current.panel.limits()$ylim),
             type = "p",
             ...)
{
    if (ref)
    {
        reference.line <- trellis.par.get("reference.line")
        panel.abline(h = 0,
                     col = reference.line$col,
                     lty = reference.line$lty,
                     lwd = reference.line$lwd)
    }
    plot.line <- trellis.par.get("plot.line")
    superpose.line <- trellis.par.get("superpose.line")
    if (!is.null(groups))
    {
        panel.superpose(x, darg = darg,
                        plot.points = plot.points, ref = FALSE,
                        groups = groups,
                        panel.groups = panel.densityplot,
                        jitter.amount = jitter.amount,
                        type = type,
                        ...)
    }
    else
    {
        if (sum(!is.na(x)) > 1)
        {
            h <- do.call("density", c(list(x = x), darg))
            lim <- current.panel.limits()$xlim
            id <- h$x > lim[1] & h$x < lim[2]
            panel.lines(x = h$x[id], y = h$y[id], ...)
        }
        switch(as.character(plot.points),
               "TRUE" =
               panel.xyplot(x = x, y = rep(0, length(x)), type = type, ...),
               "rug" =
               panel.rug(x = x, 
                         start = 0, end = 0,
                         x.units = c("npc", "native"),
                         type = type,
                         ...),
               "jitter" =
               panel.xyplot(x = x,
                            y = jitter(rep(0, length(x)), amount = jitter.amount),
                            type = type,
                            ...))
    }
}



densityplot <- function(x, data, ...) UseMethod("densityplot")


densityplot.numeric <-
    function(x, data = NULL, xlab = deparse(substitute(x)), ...)
{
    ocall <- ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit data specification ignored")
    ccall$data <- list(x = x)
    ccall$xlab <- xlab
    ccall$x <- ~x
    ccall[[1]] <- as.name("densityplot")
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}



densityplot.formula <-
    function(x,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = !is.null(groups),
##              allow.multiple = is.null(groups) || outer,
##              outer = FALSE,
             auto.key = FALSE,
             aspect = "fill",
             panel = "panel.densityplot",
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
             na.rm = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(),
             subscripts = !is.null(groups),
             subset = TRUE)
{
    formula <- x
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
    
    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))

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
    x <- form$right
    if (length(cond) == 0) {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }

    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- gettext("Density")

    ## if (!is.numeric(x))
    ##     warning("x should be numeric")
    ## x <- as.numeric(x)

    ## create a skeleton trellis object with the
    ## less complicated components:
    foo <-
        do.call("trellis.skeleton",
                c(list(formula = formula, 
                       cond = cond,
                       aspect = aspect,
                       strip = strip,
                       panel = panel,
                       xlab = xlab,
                       ylab = ylab,
                       xlab.default = form$right.name,
                       ylab.default = gettext("Density")), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))

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
    if (have.xlog)
    {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)
        
        x <- log(x, xbase)
        if (have.xlim) xlim <- log(xlim, xbase)
    }
    if (have.ylog)
    {
        warning("Can't have log Y-scale")
        have.ylog <- FALSE
        foo$y.scales$log <- FALSE
    }

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <- c(dots, list(darg = darg))
    if (subscripts)
    {
        foo$panel.args.common$groups <- groups
    }

    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length))) 
        stop("mismatch in number of packets")
    foo$panel.args <- vector(mode = "list", length = npackets)

    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1)
    {
        dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
        dimnames(foo$packet.sizes) <- lapply(foo$condlevels, as.character)
    }

    cond.current.level <- rep(1, length(cond))

    for (packet.number in seq(length = npackets))
    {
        id <- compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)

        foo$panel.args[[packet.number]] <- list(x = x[id])
        if (subscripts)
            foo$panel.args[[packet.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }


    more.comp <-
        c(limits.and.aspect(prepanel.default.densityplot,
                            prepanel = prepanel, 
                            have.xlim = have.xlim, xlim = xlim, 
                            have.ylim = have.ylim, ylim = ylim, 
                            x.relation = foo$x.scales$relation,
                            y.relation = foo$y.scales$relation,
                            panel.args.common = foo$panel.args.common,
                            panel.args = foo$panel.args,
                            aspect = aspect,
                            npackets = npackets,
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
                                      points = FALSE,
                                      rectangles = FALSE,
                                      lines = TRUE),
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


