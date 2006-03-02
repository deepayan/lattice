

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

    if (n.r > 1)
        panel.segments(x0 = 0, x1 = 1,
                       y0 = seq(length = n.r),
                       y1 = seq(length = n.r),
                       col = reference.line$col,
                       lwd = reference.line$lwd,
                       lty = reference.line$lty)
    else return(invisible())

    if (is.null(groups))
    {
        for (i in seq(length = n.r-1))
        {
            x0 <- (as.numeric(z[subscripts, i, ]) - lower[i])/dif[i]
            x1 <- (as.numeric(z[subscripts, i+1, ]) - lower[i+1])/dif[i+1]
            panel.segments(x0 = x0, y0 = i, x1 = x1, y1 = i + 1,
                           col = col,
                           lty = lty,
                           lwd = lwd,
                           alpha = alpha,
                           ...)
        }        
##         for (i in seq(along=subscripts))
##         {
##             x <- (as.numeric(z[subscripts[i],,])-lower)/dif
##             grid.lines(x = x,
##                        y = 1:n.r, 
##                        gp =
##                        gpar(col = col[i],
##                             lty = lty[i],
##                             lwd = lwd[i],
##                             alpha = alpha[i]),
##                        default.units = "native")
##         }
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




parallel <- function(x, data, ...) UseMethod("parallel")

## {
##     ocall <- match.call()
##     formula <- ocall$formula
##     if (!is.null(formula))
##     {
##         warning("The 'formula' argument has been renamed to 'x'. See ?xyplot")
##         ocall$formula <- NULL
##         if (!("x" %in% names(ocall))) ocall$x <- formula else warning("'formula' overridden by 'x'")
##         eval(ocall, parent.frame())
##     }
##     else UseMethod("parallel")
## }


parallel.data.frame <-
    function(x, data = NULL, ...)
{
    ocall <- ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit data specification ignored")
    ccall$data <- list(x = x)
    ccall$x <- ~x
    ccall[[1]] <- as.name("parallel")
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}


parallel.formula <-
    function(x,
             data = NULL,
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
    formula <- x

    ## dots <- eval(substitute(list(...)), data, environment(formula))
    dots <- list(...)

    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    ## right.name <- deparse(substitute(x))
    ## x <- eval(substitute(x), data, environment(formula))
    form <-
        latticeParseFormula(formula, data,
                            subset = subset, groups = groups,
                            multiple = FALSE,
                            outer = FALSE, subscripts = TRUE,
                            drop = drop.unused.levels)


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
    ## number.of.cond <- length(cond)
    x <- as.data.frame(form$right)

    if (length(cond) == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, nrow(x))))
        ## number.of.cond <- 1
    }

    if (!missing(varnames)) colnames(x) <-
        eval(substitute(varnames), data, environment(formula))

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <-
        do.call("trellis.skeleton",
                c(list(formula = formula, 
                       cond = cond,
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
    
    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))
    
    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit))
    {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit))
    {
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

    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(z = x, groups = groups), dots)


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

        foo$panel.args[[packet.number]] <-
            list(subscripts = subscr[id])

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    more.comp <-
        c(limits.and.aspect(prepanel.default.parallel,
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

    class(foo) <- "trellis"
    foo
}



