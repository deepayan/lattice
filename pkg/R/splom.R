

### Copyright 2001-2004  Deepayan Sarkar <deepayan@stat.wisc.edu>
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





prepanel.default.splom <-
    function(z, ...)
{
    list(xlim = c(.5, ncol(z) + .5),
         ylim = c(.5, ncol(z) + .5),
         dx = 1,
         dy = 1)
}




panel.splom <-
    function(...)
    panel.xyplot(...)











diag.panel.splom <-
    function(varname = NULL, limits, at = NULL, lab = NULL,
             draw = TRUE,

             varname.col = add.text$col,
             varname.cex = add.text$cex,
             varname.font = add.text$font,
             varname.fontfamily = add.text$fontfamily,
             varname.fontface = add.text$fontface,

             axis.text.col = axis.text$col,
             axis.text.alpha = axis.text$alpha,
             axis.text.cex = axis.text$cex,
             axis.text.font = axis.text$font,
             axis.text.fontfamily = axis.text$fontfamily,
             axis.text.fontface = axis.text$fontface,

             axis.line.col = axis.line$col,
             axis.line.alpha = axis.line$alpha,
             axis.line.lty = axis.line$lty,
             axis.line.lwd = axis.line$lwd,
             ...)
{

    add.text <- trellis.par.get("axis.line")
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")

    if (!is.null(varname))
        grid.text(varname,
                  gp =
                  gpar(col = varname.col,
                       cex = varname.cex,
                       fontface = chooseFace(varname.fontface, varname.font),
                       fontfamily = varname.fontfamily))

    if (draw) {
        ## plot axes

        rot <- c(90, 0)
        if (is.null(at))
        {
            at <- 
                if (is.character(limits)) seq(along = limits)
                else pretty(limits)
        }
        if (is.null(lab))
        {
            lab <- 
                if (is.character(limits)) limits
                else {
                    rot <- 0
                    format(at, trim = TRUE)
                }
        }
        for (side in c("left", "top", "right", "bottom"))
            panel.axis(side = side,
                       at = at,
                       labels = lab,
                       tick = TRUE,
                       half = TRUE,

                       tck = 1, ## from scales ?
                       rot = rot, 

                       text.col = axis.text.col,
                       text.alpha = axis.text.alpha,
                       text.cex = axis.text.cex,
                       text.font = axis.text.font,
                       text.fontfamily = axis.text.fontfamily,
                       text.fontface = axis.text.fontface,

                       line.col = axis.line.col,
                       line.alpha = axis.line.alpha,
                       line.lty = axis.line.lty,
                       line.lwd = axis.line.lwd)
    }
}

















panel.pairs <-
    function(z,
             panel = "panel.splom",
             lower.panel = panel,
             upper.panel = panel,
             diag.panel = "diag.panel.splom",
             as.matrix = FALSE,

             groups = NULL,
             panel.subscripts,
             subscripts,
             pscales = 5,

             panel.number = 0,  ## should always be supplied
             panel.counter = 0,  ## should always be supplied

             prepanel.limits = function(x) if (is.factor(x)) levels(x) else
             extend.limits(range(as.numeric(x), na.rm = TRUE)),

             varname.col = add.text$col,
             varname.cex = add.text$cex,
             varname.font = add.text$font,
             varname.fontfamily = add.text$fontfamily,
             varname.fontface = add.text$fontface,

             axis.text.col = axis.text$col,
             axis.text.cex = axis.text$cex,
             axis.text.font = axis.text$font,
             axis.text.fontfamily = axis.text$fontfamily,
             axis.text.fontface = axis.text$fontface,

             axis.line.col = axis.line$col,
             axis.line.lty = axis.line$lty,
             axis.line.lwd = axis.line$lwd,

             ...)
{
    lower.panel <- 
        if (is.function(lower.panel)) lower.panel 
        else if (is.character(lower.panel)) get(lower.panel)
        else eval(lower.panel)

    upper.panel <- 
        if (is.function(upper.panel)) upper.panel 
        else if (is.character(upper.panel)) get(upper.panel)
        else eval(upper.panel)

    diag.panel <- 
        if (is.function(diag.panel)) diag.panel 
        else if (is.character(diag.panel)) get(diag.panel)
        else eval(diag.panel)

    add.text <- trellis.par.get("axis.line")
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")

    n.var <- ncol(z)

    if (n.var == 0) return()

    lim <- vector("list", length = n.var)
    for(i in seq(length = n.var)) lim[[i]] <-
        if (is.list(pscales) && !is.null(pscales[[i]]$lim))
            pscales[[i]]$lim
        else prepanel.limits(z[,i])
    
    ## maybe (ideally) this should be affected by scales

    if (any(subscripts)) {

        draw <- is.list(pscales) || (is.numeric(pscales) && pscales!=0) # whether axes to be drawn
        splom.layout <- grid.layout(nrow = n.var, ncol = n.var)
        pushViewport(viewport(layout = splom.layout, name = "pairs"))

        for(i in 1:n.var)     ## i = row, j = col
            for(j in 1:n.var)
            {
                if (as.matrix)
                    pushViewport(viewport(layout.pos.row = i,
                                          layout.pos.col = j,
                                          name = paste("subpanel", i, j, sep = "."),
                                          clip = trellis.par.get("clip")$panel,
                                          xscale = if (is.character(lim[[j]]))
                                          c(0, length(lim[[j]]) + 1) else lim[[j]],
                                          yscale = if (is.character(lim[[i]]))
                                          c(0, length(lim[[i]]) + 1) else lim[[i]]))
                else
                    pushViewport(viewport(layout.pos.row = n.var - i + 1,
                                          layout.pos.col = j,
                                          name = paste("subpanel", i, j, sep = "."),
                                          clip = trellis.par.get("clip")$panel,
                                          xscale = if (is.character(lim[[j]]))
                                          c(0, length(lim[[j]]) + 1) else lim[[j]],
                                          yscale = if (is.character(lim[[i]]))
                                          c(0, length(lim[[i]]) + 1) else lim[[i]]))
                if(i == j)
                {
                    axls <-
                        if (is.list(pscales) && !is.null(pscales[[i]]$at))
                            pscales[[i]]$at
                        else if (is.character(lim[[i]]))
                            seq(along = lim[[i]])
                        else
                            pretty(lim[[i]],
                                   n = if (is.numeric(pscales))
                                   pscales else 5)

                    labels <-
                        if (is.list(pscales) && !is.null(pscales[[i]]$lab))
                            pscales[[i]]$lab
                        else if (is.character(lim[[i]]))
                            lim[[i]]
                        else
                            NULL

                    if (is.numeric(lim[[i]]))
                    {
                        axlims <- range(lim[[i]])
                        axid <- axls > axlims[1] & axls < axlims[2]
                        axls <- axls[axid]
                        labels <- labels[axid]
                    }
                        

                    diag.panel(varname = colnames(z)[i],
                               limits = lim[[i]],
                               at = axls, lab = labels,
                               draw = draw,

                               varname.col = varname.col,
                               varname.cex = varname.cex,
                               varname.font = varname.font,
                               varname.fontfamily = varname.fontfamily,
                               varname.fontface = varname.fontface,

                               axis.text.col = axis.text.col,
                               axis.text.cex = axis.text.cex,
                               axis.text.font = axis.text.font,
                               axis.text.fontfamily = axis.text.fontfamily,
                               axis.text.fontface = axis.text.fontface,

                               axis.line.col = axis.line.col,
                               axis.line.lty = axis.line.lty,
                               axis.line.lwd = axis.line.lwd)

                    grid.rect(gp =
                              gpar(col = axis.line.col,
                                   lty = axis.line.lty,
                                   lwd = axis.line.lwd))

                }
                else
                {
                    pargs <-
                        if (!panel.subscripts)
                            c(list(x = as.numeric(z[subscripts, j]),
                                   y = as.numeric(z[subscripts, i]),
                                   panel.number = panel.number),
                              list(...))
                        else
                            c(list(x = as.numeric(z[subscripts, j]),
                                   y = as.numeric(z[subscripts, i]),
                                   groups = groups,
                                   subscripts = subscripts,
                                   panel.number = panel.number),
                              list(...))

                    if (!("..." %in% names(formals(panel))))
                        pargs <- pargs[names(formals(panel))]

                    if (as.matrix)
                        do.call(if (i > j) "lower.panel" else "upper.panel",
                                pargs)
                    else
                        do.call(if (i < j) "lower.panel" else "upper.panel",
                                pargs)

                    grid.rect(gp =
                              gpar(col = axis.line.col,
                                   lty = axis.line.lty,
                                   lwd = axis.line.lwd))
                }
                upViewport()
            }
        upViewport()
    }
}




splom <-
    function(formula,
             data = parent.frame(),
             auto.key = FALSE,
             aspect = 1,
             between = list(x = 0.5, y = 0.5),
             panel = if (is.null(groups)) "panel.splom" else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = "Scatter Plot Matrix",
             xlim,
             ylab = NULL,
             ylim,
             superpanel = "panel.pairs",
             pscales = 5,
             varnames,
             drop.unused.levels = TRUE,
             ...,
             default.scales = list(draw = FALSE, relation = "same", axs = "i"),
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
                          panel = superpanel,
                          strip = strip,
                          xlab = xlab,
                          ylab = ylab,
                          xlab.default = "Scatter Plot Matrix"), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## FIXME: It is not very clear exactly what effect scales is
    ## supposed to have. Not much in Trellis (probably), but there are
    ## certain components which are definitely relevant, and certain
    ## others (like log) which can be used in innovative
    ## ways. However, I'm postponing all that to later, if at all

    if (!is.list(scales)) scales <- list()

    ## some defaults for scales

#     if (is.null(scales$draw)) scales$draw <- FALSE
#     if (is.null(scales$relation)) scales$relation <- "same"
#     if (is.null(scales$axs)) scales$axs <- "i"

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
    
    ## Step 4: Decide if log scales are being used (has to be NO):

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log

    ## immaterial, since scales has no effect.

#    if (have.xlog) {
#        xlog <- foo$x.scales$log
#        xbase <-
#            if (is.logical(xlog)) 10
#            else if (is.numeric(xlog)) xlog
#            else if (xlog == "e") exp(1)
#
#        x <- log(x, xbase)
#        if (have.xlim) xlim <- log(xlim, xbase)
#    }
#    if (have.ylog) {
#        ylog <- foo$y.scales$log
#        ybase <-
#            if (is.logical(ylog)) 10
#            else if (is.numeric(ylog)) ylog
#            else if (ylog == "e") exp(1)
#
#        y <- log(y, ybase)
#        if (have.ylim) ylim <- log(ylim, ybase)
#    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## id.na used only to see if any plotting is needed. Not used
    ## subsequently, unlike other functions

    id.na <- FALSE
    for (j in 1:ncol(x))
        id.na <- id.na | is.na(x[,j])
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?


    ## Step 6: Evaluate layout, panel.args.common and panel.args


    foo$panel.args.common <-
        c(list(z = x,
               panel = panel,
               panel.subscripts = TRUE,
               groups = groups, # xscales = foo$x.scales, yscales = foo$y.scales,
               pscales = pscales),
          dots)

    nplots <- prod(cond.max.level)
    if (nplots != prod(sapply(foo$condlevels, length))) stop("mismatch")
    foo$panel.args <- vector(mode = "list", length = nplots)


    cond.current.level <- rep(1, number.of.cond)


    for (panel.number in seq(length = nplots))
    {

        ##id <- !id.na  WHY ?
        for(i in 1:number.of.cond)
        {
            var <- cond[[i]]
            id <- if (is.shingle(var))
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


    more.comp <- c(limits.and.aspect(prepanel.default.splom,
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

