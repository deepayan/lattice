


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



prepanel.default.qq <-
    function(x, y, ...)
{
    if (!is.numeric(x)) x <- as.numeric(x)
    if (!is.numeric(y)) y <- as.numeric(y)

    list(xlim = range(x, y),
         ylim = range(x, y),
         dx = 1,
         dy = 1)
}




panel.qq <-
    function(...)
{
    reference.line <- trellis.par.get("reference.line")
    panel.abline(0,1,
                 col = reference.line$col,
                 lty = reference.line$lty,
                 lwd = reference.line$lwd)
    panel.xyplot(...)

}



qq <- function(x, ...)
{
    ocall <- match.call()
    formula <- ocall$formula
    if (!is.null(formula))
    {
        warning("The 'formula' argument has been renamed to 'x'. See ?xyplot")
        ocall$formula <- NULL
        if (is.null(ocall$x)) ocall$x <- formula
        eval(ocall, parent.frame())
    }
    else UseMethod("qq")
}




qq.formula <-
    function(x,
             data = parent.frame(),
             aspect = "fill",
             panel = "panel.qq",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             f.value = NULL,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(),
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <-
        latticeParseFormula(x, data, subset = subset,
                            groups = groups, subscripts = TRUE,
                            drop = drop.unused.levels)

    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if(subscripts) subscr <- form$subscr

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
        number.of.cond <- 1
    }

    ##x <- as.numeric(x)
    y <- as.factorOrShingle(y)
    is.f.y <- is.factor(y)
    num.l.y <- nlevels(y)
    if (num.l.y!=2) stop("y must have exactly 2 levels")

    if(missing(xlab)) xlab <-
        if (is.f.y) unique(levels(y))[1]
        else paste("y:", as.character(unique(levels(y)[[1]])))
    
    if(missing(ylab)) ylab <-
        if (is.f.y) unique(levels(y))[2]
        else paste("y:", as.character(unique(levels(y)[[2]])))


    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(cond = cond,
                          aspect = aspect,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab,
                          xlab.default =
                          if (is.f.y) unique(levels(y))[1]
                          else paste("y:", as.character(unique(levels(y)[[1]]))),

                          ylab.default =
                          if (is.f.y) unique(levels(y))[y]
                          else paste("y:", as.character(unique(levels(y)[[2]])))),
                     dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## scales <- eval(substitute(scales), data, parent.frame())
    if (is.character(scales)) scales <- list(relation = scales)
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

    ## Step 4: Decide if log scales are being used: completed later

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        ## x <- log(x, xbase)  later, in panel.args
        if (have.xlim) xlim <- log(xlim, xbase)
    }
    if (have.ylog) {
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        ## y <- log(y, ybase)
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
    if (subscripts) foo$panel.args.common$groups <- groups


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

        if (any(id)) {

            if (is.f.y) {
                tx <- x[id]
                ty <- as.numeric(y[id])
                x.val <- tx[ty==1]
                y.val <- tx[ty==2]
            }
            else {
                tx <- x[id]
                ty <- y[id]
                ly <- levels(y)
                x.val <- tx[ty>=ly[[1]][1] & ty <=ly[[1]][2]]
                y.val <- tx[ty>=ly[[2]][1] & ty <=ly[[2]][2]]
            }
            n <- max(length(x.val), length(y.val))
            ## changed from S-PLUS, where f.value = ppoints is default
            p  <-
                if (is.null(f.value)) ppoints(n, a = 1)
                else if (is.numeric(f.value)) f.value
                else f.value(n)
            foo$panel.args[[panel.number]] <-
                list(x = quantile(x = x.val, probs = p),
                     y = quantile(x = y.val, probs = p))

        }
        else
            foo$panel.args[[panel.number]] <-
                list(x = numeric(0), y = numeric(0))

        if (subscripts)
            foo$panel.args[[panel.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    more.comp <- c(limits.and.aspect(prepanel.default.qq,
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

    class(foo) <- "trellis"
    foo
}
