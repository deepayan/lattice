

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



prepanel.default.parallel <-
    function(x, y, z, ...)
{
    list(xlim = c(0,1),
         ylim = extend.limits(c(1, ncol(as.data.frame(z))), prop = 0.03), 
         ##ylim = colnames(as.data.frame(z)),
         dx = 1,
         dy = 1)
}



panel.parallel <-
    function(x, y, z, subscripts,
             groups = NULL,
             col = superpose.line$col,
             lwd = superpose.line$lwd,
             lty = superpose.line$lty,
             alpha = superpose.line$alpha,
             common.scale = TRUE,
             lower = sapply(z, function(x) min(as.numeric(x), na.rm = TRUE)),
             upper = sapply(z, function(x) max(as.numeric(x), na.rm = TRUE)),
             ...)
{
    superpose.line <- trellis.par.get("superpose.line")
    reference.line <- trellis.par.get("reference.line")

    n.r <- ncol(z)
    n.c <- length(subscripts)
    if (is.null(groups))
    {
        col <- rep(col, length = n.c)
        lty <- rep(lty, length = n.c)
        lwd <- rep(lwd, length = n.c)
        alpha <- rep(alpha, length = n.c)
    }
    else
    {
        gnum <- as.integer(as.factor(groups))
        n.g <- length(unique(gnum))
        col <- rep(col, length = n.g)
        lty <- rep(lty, length = n.g)
        lwd <- rep(lwd, length = n.g)
        alpha <- rep(alpha, length = n.g)
    }

    if (!common.scale)
    {
        lower <- min(lower)
        upper <- max(upper)
    }
    lower <- rep(lower, length = n.r)
    upper <- rep(upper, length = n.r)
    dif <- upper - lower

    if (n.r > 0)
        panel.segments(x0 = 0, x1 = 1,
                       y0 = seq(length = n.r),
                       y1 = seq(length = n.r),
                       col = reference.line$col,
                       lwd = reference.line$lwd,
                       lty = reference.line$lty)



#     for(i in seq(length = n.r))
#     {
#         grid.segments(x0 = c(0, 1), y = c(i, i),
#                       default.units = "native",
#                       gp = gpar(col = reference.line$col,
#                       lwd = reference.line$lwd,
#                       lty = reference.line$lty))
#         lower[i] <- range(as.numeric(z[,i]))[1]
#         upper[i] <- range(as.numeric(z[,i]))[2]
        
#     }
    
    if (is.null(groups))
        for (i in seq(along=subscripts))
        {
            x <- (as.numeric(z[subscripts[i],,])-lower)/dif
            grid.lines(x = x,
                       y = 1:n.r, 
                       gp =
                       gpar(col = col[i],
                            lty = lty[i],
                            lwd = lwd[i],
                            alpha = alpha[i]),
                       default.units = "native")
        }
    else 
        for (i in seq(along = subscripts))
        {
            x <- (as.numeric(z[subscripts[i],,])-lower)/dif
            grid.lines(x = x,
                       y = 1:n.r, 
                       gp =
                       gpar(col = col[gnum[subscripts[i]]],
                            lty = lty[gnum[subscripts[i]]],
                            lwd = lwd[gnum[subscripts[i]]],
                            alpha = alpha[gnum[subscripts[i]]]),
                       default.units="native")
        }
    invisible()
}






# panel.parallel.old <- function(z, subscripts,
#                                col=superpose.line$col,
#                                lwd=superpose.line$lwd,
#                                lty=superpose.line$lty, ...)
# {

#     superpose.line <- trellis.par.get("superpose.line")
#     reference.line <- trellis.par.get("reference.line")

#     n.r <- ncol(z)
#     n.c <- length(subscripts)
#     col <- rep(col, length=n.c)
#     lty <- rep(lty, length=n.c)
#     lwd <- rep(lwd, length=n.c)

#     llim <- numeric(n.r)
#     ulim <- numeric(n.r)
#     dif <- numeric(n.r)
#     if (n.r > 0)
#         for(i in 1:n.r) {
#             grid.lines(x = c(0,1), y = c(i,i),
#                        default.units = "native",
#                        gp = gpar(col = reference.line$col,
#                        lwd = reference.line$lwd,
#                        lty = reference.line$lty))
#             llim[i] <- range(as.numeric(z[,i]))[1]
#             ulim[i] <- range(as.numeric(z[,i]))[2]
#             dif[i] <- ulim[i] - llim[i]
#         }
   

#     for (i in seq(along=subscripts))
#     {
#         x <- (as.numeric(z[subscripts[i],,])-llim)/dif
#         grid.lines(x = x,
#                    y=1:n.r, 
#                    gp = gpar(col=col[i], lty=lty[i], lwd=lwd[i]),
#                    default.units="native")
#     }
    
# }






parallel <-
    function(formula,
             data = parent.frame(),
             aspect = "fill",
             between = list(x = 0.5, y = 0.5),
             panel = "panel.parallel",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = NULL,
             xlim,
             ylab = NULL,
             ylim,
             varnames,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales =
             list(x = list(at = c(0, 1), labels = c("Min", "Max")),
                  y =
                  list(alternating = FALSE, axs = "i", tck = 0,
                       at = 1:ncol(x), labels = colnames(x))),
             subset = TRUE)
{

    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    right.name <- deparse(substitute(formula))
    formula <- eval(substitute(formula), data, parent.frame())
    form <-
        if (inherits(formula, "formula"))
            latticeParseFormula(formula, data,
                                subset = subset, groups = groups,
                                multiple = FALSE,
                                outer = FALSE, subscripts = TRUE,
                                drop = drop.unused.levels)
        else {
            if (is.matrix(formula)) {
                list(left = NULL,
                     right = as.data.frame(formula)[subset,],
                     condition = NULL,
                     left.name = "",
                     right.name =  right.name,
                     groups = groups,
                     subscr = seq(length = nrow(formula))[subset])
            }
            else if (is.data.frame(formula)) {
                list(left = NULL,
                     right = formula[subset,],
                     condition = NULL,
                     left.name = "",
                     right.name =  right.name,
                     groups = groups,
                     subscr = seq(length = nrow(formula))[subset])
            }
            else stop("invalid formula")
        }


    ## We need to be careful with subscripts here. It HAS to be there,
    ## and it's to be used to index x, y, z (and not only groups,
    ## unlike in xyplot etc). This means we have to subset groups as
    ## well, which is about the only use for the subscripts calculated
    ## in latticeParseFormula, after which subscripts is regenerated
    ## as a straight sequence indexing the variables

    if (!is.null(form$groups)) groups <-  form$groups[form$subscr]
    subscr <- seq(length = nrow(form$right))

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)


    cond <- form$condition
    number.of.cond <- length(cond)
    x <- as.data.frame(form$right)

    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, nrow(x))))
        number.of.cond <- 1
    }

    if (!missing(varnames)) colnames(x) <-
        eval(substitute(varnames), data, parent.frame())

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(cond = cond,
                          aspect = aspect,
                          between = between,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab,
                          xlab.default = "Parallel Coordinate Plot"), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## overriding at and labels, maybe not necessary
    
    ## scales <- eval(substitute(scales), data, parent.frame())

    if (is.character(scales)) scales <- list(relation = scales)

#     if (is.null(scales$alternating)) {
#         if (is.null(scales$y)) scales$y <- list(alternating = FALSE)
#         else if (is.null(scales$y$alternating)) scales$y$alternating <- FALSE
#         ## bug if y="free" but who cares
#     }




    scales <- updateList(default.scales, scales)
    foo <- c(foo, 
             do.call("construct.scales", scales))

#     ## forcing this for now. Shouldn't be too hard to give more
#     ## control to the user

#     ##foo$x.scales$at <- c(0,1)
#     ##foo$x.scales$labels <- c("Min","Max")
#     ##foo$y.scales$at <- 1:ncol(x)
#     ##foo$y.scales$labels <- 
    
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
        foo$x.scales$log <- FALSE
        ## This is because No further changes will be
        ## necessary while printing since x-axes are not
        ## marked (many x axes)
    }
    if (have.ylog) {
        warning("cannot have log y-scale")
        foo$y.scales$log <- FALSE
    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- FALSE
    for (j in 1:ncol(x))
        id.na <- id.na | is.na(x[,j])
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?


    ## Step 6: Evaluate layout, panel.args.common and panel.args


    foo$panel.args.common <-
        c(list(z = x, groups = groups), dots)


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
            list(subscripts = subscr[id])

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    more.comp <- c(limits.and.aspect(prepanel.default.parallel,
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



