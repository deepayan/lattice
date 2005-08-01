



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




## Reimplementation of qqmath




prepanel.default.qqmath <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL,
             subscripts, ...)
{
    if (!is.numeric(x)) x <- as.numeric(x)
    distribution <-
        if (is.function(distribution)) distribution 
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)
    nobs <- sum(!is.na(x))
    getxx <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            distribution(ppoints(nobs))
        else
            distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            sort(x)
        else
            quantile(x, f.value(nobs),
                     names = FALSE,
                     type = qtype,
                     na.rm = TRUE)
    }
    if (!nobs)
        list(xlim = c(NA, NA),
             ylim = c(NA, NA),
             dx = NA, dy = NA)
    else if (!is.null(groups))
    {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        list(xlim = range(unlist(xxlist), na.rm = TRUE),
             ylim = range(unlist(yylist), na.rm = TRUE),
             dx = unlist(lapply(xxlist, diff)),
             dy = unlist(lapply(yylist, diff)))
    }
    else 
    {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        list(xlim = range(xx),
             ylim = range(yy),
             dx = diff(xx),
             dy = diff(yy))
    }
}





panel.qqmath <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL, ...)
{
    x <- as.numeric(x)
    distribution <-
        if (is.function(distribution)) distribution 
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)
    nobs <- sum(!is.na(x))
    if (!is.null(groups))
        panel.superpose(x, y = NULL,
                        f.value = f.value,
                        distribution = distribution,
                        qtype = qtype,
                        groups = groups,
                        panel.groups = panel.qqmath,
                        ...)
    else if (nobs)
    {
        if (is.null(f.value)) # exact data instead of quantiles
            panel.xyplot(x = distribution(ppoints(nobs)),
                         y = sort(x),
                         ...)
        else
            panel.xyplot(x = distribution(f.value(nobs)), 
                         y =
                         quantile(x, f.value(nobs),
                                  names = FALSE,
                                  type = qtype,
                                  na.rm = TRUE),
                         ...)
    }
}





## qqmath2 <-
##     function(formula,
##              data = parent.frame(),
##              prepanel = prepanel.default.qqmath,
##              panel = "panel.qqmath",
##              groups = NULL,
##              distribution = qnorm,
##              f.value = NULL,
##              xlab = paste(deparse(substitute(distribution)), collapse = ""),
##              ylab,
##              ...,
##              subset = TRUE)
## {
##     dots <- list(...)
##     groups <- eval(substitute(groups), data, parent.frame())
##     subset <- eval(substitute(subset), data, parent.frame())

##     right.name <- deparse(substitute(formula))
##     try(formula <- eval(formula), silent = TRUE)
##     foo <- substitute(formula)

##     if (!(is.call(foo) && foo[[1]] == "~")) {
##         formula <- as.formula(paste("~", right.name))
##         environment(formula) <- parent.frame()
##     }
##     if (missing(ylab))
##     {
##         fright <- formula[[2]]
##         if (length(fright) == 3 && fright[[1]] == "|")
##             fright <- fright[[2]]
##         ylab <- paste(deparse(fright), collapse = "")
##     }
##     call.list <-
##         c(list(formula = formula,
##                data = data,
##                prepanel = prepanel,
##                panel = panel,
##                groups = groups,
##                panel.groups = NULL,
##                distribution = distribution,
##                f.value = f.value,
##                xlab = xlab,
##                ylab = ylab,
##                subset = subset),
##           dots)
##     ans <- do.call("densityplot", call.list)
##     ans$call <- match.call()
##     ans
## }




qqmath <- function(formula, ...)  UseMethod("qqmath")



qqmath.numeric <-
    function(formula, data = NULL, xlab = deparse(substitute(formula)), ...)
{
    ocall <- ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit data specification ignored")
    ccall$data <- list(x = formula)
    ccall$xlab <- xlab
    ccall$formula <- ~x
    ccall[[1]] <- as.name("qqmath")
    ans <- eval(ccall, parent.frame())
    ans$call <- ocall
    ans
}



qqmath.formula <-
    function(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = !is.null(groups),
             distribution = qnorm,
             f.value = NULL,
             auto.key = FALSE,
             aspect = "fill",
             panel = "panel.qqmath",
             ## panel = if (is.null(groups)) "panel.densityplot" else "panel.superpose",
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
    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)
    
    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    right.name <- deparse(substitute(formula))
    try(formula <- eval(formula), silent = TRUE)
    foo <- substitute(formula)

##     if (!inherits(formula, "formula"))
##         formula <- as.formula(paste("~", formname))
    
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
    if (number.of.cond == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
        number.of.cond <- 1
    }

    dist.name <- paste(deparse(substitute(distribution)), collapse = "")
    if (missing(xlab)) xlab <- dist.name
    if (missing(ylab)) ylab <- form$right.name

    ## create a skeleton trellis object with the
    ## less complicated components:
    foo <-
        do.call("trellis.skeleton",
                c(list(cond = cond,
                       aspect = aspect,
                       strip = strip,
                       panel = panel,
                       xlab = xlab,
                       ylab = ylab,
                       xlab.default = dist.name,
                       ylab.default = form$right.name),
                  dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## scales <- eval(substitute(scales), data, parent.frame())
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

    ## This is slightly weird because 'x' is eventually plotted in the
    ## Y-axis
    if (have.xlog)
    {
        warning("Can't have log X-scale")
        have.xlog <- FALSE
        foo$x.scales$log <- FALSE
    }
    if (have.ylog)
    {
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)
        
        x <- log(x, ybase)
        if (have.ylim) ylim <- log(ylim, ybase)
    }


    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))


    ## Old NA-handling:
    ##     id.na <- is.na(x)
    ##     for (var in cond)
    ##         id.na <- id.na | is.na(var)
    ##     if (!any(!id.na)) stop("nothing to draw")

    ## new NA-handling: will retain NA's in x

    id.na <- do.call("pmax", lapply(cond, is.na))
    if (!any(!id.na)) stop("nothing to draw")



    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <-
        c(list(distribution = distribution,
               f.value = f.value),
          dots)

    if (subscripts)
    {
        foo$panel.args.common$groups <- groups
    }

    nplots <- prod(cond.max.level)
    if (nplots != prod(sapply(foo$condlevels, length))) stop("mismatch")
    foo$panel.args <- vector(mode = "list", length = nplots)


    cond.current.level <- rep(1, number.of.cond)


    for (panel.number in seq(length = nplots))
    {
        id <- !id.na
        for(i in seq(length = number.of.cond))
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

    more.comp <-
        c(limits.and.aspect(prepanel.default.qqmath,
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














##############################################


panel.qqmathline <-
    function(x, y = x,
             distribution = qnorm,
             p = c(0.25, 0.75),
             qtype = 7,
             groups = NULL, 
             ...)
{
    y <- as.numeric(y)
    stopifnot(length(p) == 2)
    distribution <-
        if (is.function(distribution)) distribution 
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)
    nobs <- sum(!is.na(y))
    if (!is.null(groups))
        panel.superpose(x = y, y = NULL,
                        distribution = distribution,
                        p = p,
                        qtype = qtype,
                        groups = groups,
                        panel.groups = panel.qqmathline,
                        ...)
    else if (nobs)
    {
        yy <-
            quantile(y, p, names = FALSE,
                     type = qtype, na.rm = TRUE)
        xx <- distribution(p)
        r <- diff(yy)/diff(xx)
        panel.abline(c( yy[1]-xx[1]*r , r), ...)
    }
}


prepanel.qqmathline <-
    function(x, y = x,
             distribution = qnorm,
             p = c(0.25, 0.75),
             qtype = 7,
             groups = NULL,
             subscripts = TRUE,
             ...)
{
    ans <-
        prepanel.default.qqmath(x,
                                distribution = distribution,
                                qtype = qtype,
                                groups = groups,
                                subscripts = subscripts,
                                ...)
    y <- as.numeric(y)
    stopifnot(length(p) == 2)
    distribution <-
        if (is.function(distribution)) distribution 
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)
    nobs <- sum(!is.na(y))
    getdy <- function(x)
    {
        diff(quantile(x, p, names = FALSE,
                      type = qtype,
                      na.rm = TRUE))
    }
    dy <- 
        if (!is.null(groups)) sapply(split(y, groups[subscripts]), getdy)
        else getdy(y)
    if (!all(is.na(dy)))
    {
        ans$dy <- dy[!is.na(dy)]
        ans$dx <- rep(diff(distribution(p)), length(ans$dy))
    }
    ans
}


## panel.qqmathline.old <-
##     function (y, distribution, p = c(0.25, 0.75), ...)
## {
##     y <- as.numeric(y)
##     stopifnot(length(p) == 2)
##     if (length(y) > 0) {
##         yy <- quantile(y, p)
##         xx <- distribution(p)
##         r <- diff(yy)/diff(xx)
##         panel.abline(c(yy[1] - xx[1] * r, r), ...)
##     }
## }



## prepanel.qqmathline.old <-
##     function(y, distribution, f.value = ppoints, p = c(0.25, 0.75), ...)
## {
##     ##if (!is.numeric(y)) 
##     y <- as.numeric(y)
##     stopifnot(length(p) == 2)
##     n <- length(y)

##     if (n > 0)
##     {
##         yy <- quantile(y, p)
##         xx <- distribution(p)
##         list(xlim = range(distribution(f.value(n))),
##              ylim = range(y),
##              dx = diff(xx),
##              dy = diff(yy))
##     }
##     else
##     {
##         list(xlim = c(NA, NA), ylim = c(NA, NA), dx = 1, dy = 1)
##     }
## }


## prepanel.default.qqmath.old <-
##     function(...)
##     prepanel.default.xyplot(...)


## panel.qqmath.old <-
##     function(...) panel.xyplot(...)


## qqmath.old <-
##     function(formula,
##              data = parent.frame(),
##              allow.multiple = FALSE,
##              outer = TRUE,
##              auto.key = FALSE,
##              aspect = "fill",
##              panel = "panel.qqmath",
##              prepanel = NULL,
##              scales = list(),
##              strip = TRUE,
##              groups = NULL,
##              xlab,
##              xlim,
##              ylab,
##              ylim,
## ###          f.value = ppoints,
##              f.value = NULL,
##              distribution = qnorm,
##              drop.unused.levels = lattice.getOption("drop.unused.levels"),
##              ...,
##              default.scales = list(),
##              subscripts = !is.null(groups),
##              subset = TRUE)
## {

##     ## dots <- eval(substitute(list(...)), data, parent.frame())
##     dots <- list(...)

##     groups <- eval(substitute(groups), data, parent.frame())
##     subset <- eval(substitute(subset), data, parent.frame())

##     ## Step 1: Evaluate x, y, etc. and do some preprocessing

##     form <-
##         latticeParseFormula(formula, data, subset = subset,
##                             groups = groups, multiple = allow.multiple,
##                             outer = outer, subscripts = TRUE,
##                             drop = drop.unused.levels)

##     groups <- form$groups

##     if (!is.function(panel)) panel <- eval(panel)
##     if (!is.function(strip)) strip <- eval(strip)
    
##     if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
##     if (subscripts) subscr <- form$subscr

##     prepanel <-
##         if (is.function(prepanel)) prepanel
##         else if (is.character(prepanel)) get(prepanel)
##         else eval(prepanel)

##     distribution.name <- deparse(substitute(distribution))
##     distribution <-
##         if (is.function(distribution)) distribution 
##         else if (is.character(distribution)) get(distribution)
##         else eval(distribution)

##     cond <- form$condition
##     number.of.cond <- length(cond)
##     x <- form$right

##     if (number.of.cond == 0) {
##         strip <- FALSE
##         cond <- list(as.factor(rep(1, length(x))))
##         number.of.cond <- 1
##     }



    



##     if(missing(ylab)) ylab <- form$right.name
##     if(missing(xlab)) xlab <- distribution.name
##     if (is.shingle(x)) stop("x cannot be a shingle")
##     ##x <- as.numeric(x)

##     ## create a skeleton trellis object with the
##     ## less complicated components:

##     foo <- do.call("trellis.skeleton",
##                    c(list(cond = cond,
##                           aspect = aspect,
##                           strip = strip,
##                           panel = panel,
##                           xlab = xlab,
##                           ylab = ylab,
##                           xlab.default = distribution.name,
##                           ylab.default = form$right.name,
##                           distribution = distribution),
##                      dots))

##     dots <- foo$dots # arguments not processed by trellis.skeleton
##     foo <- foo$foo
##     foo$call <- match.call()

##     ## Step 2: Compute scales.common (leaving out limits for now)

##     ## scales <- eval(substitute(scales), data, parent.frame())
##     if (is.character(scales)) scales <- list(relation = scales)
##     scales <- updateList(default.scales, scales)
##     foo <- c(foo, 
##              do.call("construct.scales", scales))


##     ## Step 3: Decide if limits were specified in call:

##     have.xlim <- !missing(xlim)
##     if (!is.null(foo$x.scales$limit)) {
##         have.xlim <- TRUE
##         xlim <- foo$x.scales$limit
##     }
##     have.ylim <- !missing(ylim)
##     if (!is.null(foo$y.scales$limit)) {
##         have.ylim <- TRUE
##         ylim <- foo$y.scales$limit
##     }

##     ## Step 4: Decide if log scales are being used:

##     have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
##     have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
##     if (have.xlog)
##     {
##         xlog <- foo$x.scales$log
##         xbase <-
##             if (is.logical(xlog)) 10
##             else if (is.numeric(xlog)) xlog
##             else if (xlog == "e") exp(1)

##         x <- log(x, xbase)
##         if (have.xlim) xlim <- log(xlim, xbase)
##     }
##     if (have.ylog) {
##         warning("cannot use log scale for y, use different distribution")
##         foo$y.scales$log <- FALSE
##     }
    
##     ## Step 5: Process cond

##     cond.max.level <- unlist(lapply(cond, nlevels))


##     id.na <- is.na(x)
##     for (var in cond)
##         id.na <- id.na | is.na(var)
##     if (!any(!id.na)) stop("nothing to draw")
##     ## Nothing simpler ?


##     ## Step 6: Evaluate layout, panel.args.common and panel.args

##     foo$panel.args.common <- dots
##     if (subscripts) foo$panel.args.common$groups <- groups

##     nplots <- prod(cond.max.level)
##     if (nplots != prod(sapply(foo$condlevels, length))) stop("mismatch")
##     foo$panel.args <- vector(mode = "list", length = nplots)


##     cond.current.level <- rep(1, number.of.cond)


##     for (panel.number in seq(length = nplots))
##     {

##         id <- !id.na
##         for(i in 1:number.of.cond)
##         {
##             var <- cond[[i]]
##             id <- id &
##             if (is.shingle(var))
##                 ((var >=
##                   levels(var)[[cond.current.level[i]]][1])
##                  & (var <=
##                     levels(var)[[cond.current.level[i]]][2]))
##             else (as.numeric(var) == cond.current.level[i])
##         }
##         if (any(id))
##         {
##             foo$panel.args[[panel.number]] <-
##                 if (is.null(f.value)) # exact data instead of quantiles
##                     list(x = distribution(ppoints(length(x[id]))), 
##                          y = sort(x[id]))
##                 else
##                     list(x = distribution(f.value(length(x[id]))), 
##                          y = quantile(x[id], f.value(length(x[id]))))
##         }
##         else
##             foo$panel.args[[panel.number]] <-
##                 list(x = numeric(0), y = numeric(0))

##         if (subscripts)
##             foo$panel.args[[panel.number]]$subscripts <-
##                 subscr[id]

##         cond.current.level <-
##             cupdate(cond.current.level,
##                     cond.max.level)
##     }


##     more.comp <- c(limits.and.aspect(prepanel.default.qqmath,
##                                      prepanel = prepanel, 
##                                      have.xlim = have.xlim, xlim = xlim, 
##                                      have.ylim = have.ylim, ylim = ylim, 
##                                      x.relation = foo$x.scales$relation,
##                                      y.relation = foo$y.scales$relation,
##                                      panel.args.common = foo$panel.args.common,
##                                      panel.args = foo$panel.args,
##                                      aspect = aspect,
##                                      nplots = nplots,
##                                      x.axs = foo$x.scales$axs,
##                                      y.axs = foo$y.scales$axs),
##                    cond.orders(foo))
##     foo[names(more.comp)] <- more.comp

##     if (is.null(foo$legend) && !is.null(groups) &&
##         (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
##     {
##         foo$legend <-
##             list(list(fun = "drawSimpleKey",
##                       args =
##                       updateList(list(text = levels(as.factor(groups)),
##                                       points = TRUE,
##                                       rectangles = FALSE,
##                                       lines = FALSE), 
##                                  if (is.list(auto.key)) auto.key else list())))
##         foo$legend[[1]]$x <- foo$legend[[1]]$args$x
##         foo$legend[[1]]$y <- foo$legend[[1]]$args$y
##         foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

##         names(foo$legend) <- 
##             if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
##                 "inside"
##             else
##                 "top"
##         if (!is.null(foo$legend[[1]]$args$space))
##             names(foo$legend) <- foo$legend[[1]]$args$space
##     }

##     class(foo) <- "trellis"
##     foo
## }





