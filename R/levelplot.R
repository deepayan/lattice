


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







prepanel.default.levelplot <-
    function(x, y, subscripts, ...)
{
    pad <- lattice.getOption("axis.padding")$numeric
    if (length(subscripts) > 0)
    {
        x <- x[subscripts]
        y <- y[subscripts]

        if (is.numeric(x))
        {
            ux <- sort(unique(x[is.finite(x)]))

            if ((ulen <- length(ux)) < 2) xlim <- ux + c(-1, 1)
            else
            {
                ## need to be careful here for DateTime classes
                diffs <- diff(as.numeric(ux))[c(1, ulen-1)]
                xlim <- c(ux[1] - diffs[1] / 2,
                          ux[ulen] + diffs[2] / 2)
            }
        }

        if (is.numeric(y)) {
            uy <- sort(unique(y[is.finite(y)]))

            if ((ulen <- length(uy)) < 2) ylim <- uy + c(-1, 1)
            else
            {
                ## need to be careful here for DateTime classes
                diffs <- diff(as.numeric(uy))[c(1, ulen-1)]
                ylim <- c(uy[1] - diffs[1] / 2,
                          uy[ulen] + diffs[2] / 2)
            }
        }
        list(xlim = if (is.numeric(x)) extend.limits(xlim, prop = -pad/(1 + 2*pad)) 
             else levels(x),               ##   ^^ these get extended back later

             ylim = if (is.numeric(y)) extend.limits(ylim, prop = -pad/(1 + 2*pad))
             else levels(y),

             dx = if (is.numeric(x)) length(ux) else 1,
             dy = if (is.numeric(y)) length(uy) else 1)
    }
    else
        list(xlim = c(NA, NA),
             ylim = c(NA, NA),
             dx = NA, dy = NA)
}
    










## FIXME: old obsolete version, keeping around for a while, just in  case

# prepanel.default.levelplot <-
#     function(x, y, subscripts, ...)
# {
#     if (is.numeric(x)) {
#         x <- as.numeric(x[subscripts])
#         ux <- sort(unique(x[!is.na(x)]))
#         xlim <-
#             if (length(ux) < 2) ux + c(-1, 1)
#             else c(3 * ux[1] - ux[2], 3 * ux[length(ux)] - ux[length(ux)-1])/2
#     }
#     else x <- x[subscripts]
#     if (is.numeric(y)) {
#         y <- as.numeric(y[subscripts])
#         uy <- sort(unique(y[!is.na(y)]))
#         ylim <-
#             if (length(uy) < 2) uy + c(-1, 1)
#             else c(3 * uy[1] - uy[2], 3 * uy[length(uy)] - uy[length(uy)-1])/2
#     }
#     else y <- y[subscripts]
#     list(xlim =
#          if (is.numeric(x)) extend.limits(xlim, prop = -0.0614) ## these get extended back later
#          else levels(x),
#          ylim = if (is.numeric(y)) extend.limits(ylim, prop = -0.0614)
#          else levels(y),
#          dx = if (is.numeric(x)) length(ux) else 1,
#          dy = if (is.numeric(y)) length(uy) else 1)
# }






panel.contourplot <- function(...) panel.levelplot(...)


## new version using contourLines, and hopefully to work for missing
## matrix entries as well

panel.levelplot <-
    function(x, y, z, 
             subscripts,
             at = pretty(z),
             shrink,
             labels = NULL,
             label.style = c("mixed", "flat", "align"),
             contour = FALSE,
             region = TRUE,
             col = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd,
             cex = add.text$cex,
             font = add.text$font,
             fontfamily = add.text$fontfamily,
             fontface = add.text$fontface,
             col.text = add.text$col,
             ...,
             col.regions = regions$col,
             alpha.regions = regions$alpha)
{
    regions <- trellis.par.get("regions")
    numcol <- length(at) - 1
    numcol.r <- length(col.regions)
    col.regions <-
        if (numcol.r <= numcol)
            rep(col.regions, length = numcol)
        else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]
    zcol <- rep(NA, length(z)) #numeric(length(z))
    for (i in seq(along = col.regions))
        zcol[!is.na(x) & !is.na(y) & !is.na(z) & z>=at[i] & z<at[i+1]] <- i

    label.style <- match.arg(label.style)
    x <- as.numeric(x[subscripts])
    y <- as.numeric(y[subscripts])
    minXwid <- min(diff(sort(unique(x))))
    minYwid <- min(diff(sort(unique(y))))

    fullZrange <- range(as.numeric(z), na.rm = TRUE) # for shrinking
    z <- as.numeric(z[subscripts])
    zcol <- as.numeric(zcol[subscripts])

    ## Do we need a zlim-like argument ?

    shrinkx <- c(1, 1)
    shrinky <- c(1, 1)
    if (!missing(shrink)) {
        if (is.numeric(shrink)) {
            shrinkx <- rep(shrink, length = 2)
            shrinky <- rep(shrink, length = 2)
        }
        else if (is.list(shrink)) {
            shrinkx <- rep(shrink[[1]], length = 2)
            shrinky <- rep(shrink[[1]], length = 2)
            if ("x" %in% names(shrink)) shrinkx <- rep(shrink$x, length = 2)
            if ("y" %in% names(shrink)) shrinky <- rep(shrink$y, length = 2)
        }
        else warning("Invalid shrink, ignored")
    }

    scaleWidth <- function(z, min = .8, max = .8, zl = range(z, na.rm = TRUE)) {
        if (diff(zl) == 0) rep(.5 * (min + max), length(z))
        else min + (max - min) * (z - zl[1]) / diff(zl)
    }

    
    if (any(subscripts)) {

        ## sorted unique values of x 
        ux <- sort(unique(x[!is.na(x)]))
        ## actual box boundaries (x axis)
        bx <-
            if (length(ux) > 1)
                c(3 * ux[1] - ux[2],
                  ux[-length(ux)] + ux[-1],
                  3 * ux[length(ux)] - ux[length(ux)-1]) / 2
            else
                ux + c(-.5, .5) * minXwid
        ## dimension of rectangles
        lx <- diff(bx)
        ## centers of rectangles
        cx <- (bx[-1] + bx[-length(bx)])/2

        ## same things for y
        uy <- sort(unique(y[!is.na(y)]))
        by <-
            if (length(uy) > 1)
                c(3 * uy[1] - uy[2],
                  uy[-length(uy)] + uy[-1],
                  3 * uy[length(uy)] - uy[length(uy)-1]) / 2
            else
                uy + c(-.5, .5) * minYwid
        ly <- diff(by)
        cy <- (by[-1] + by[-length(by)])/2


        idx <- match(x, ux)
        idy <- match(y, uy)

        if (region) 
            grid.rect(x = cx[idx],
                      y = cy[idy],
                      width = lx[idx] * scaleWidth(z, shrinkx[1], shrinkx[2], fullZrange),
                      height = ly[idy] * scaleWidth(z, shrinky[1], shrinky[2], fullZrange),
                      default.units = "native",
                      gp = gpar(fill=col.regions[zcol], col = NULL, alpha = alpha.regions))

        if (contour)
        {
            ## Processing the labels argument
            if (is.logical(labels) && !labels) labels <- NULL
            else
            {
                if (is.logical(labels)) labels <- format(at, trim = TRUE)
                text <- trellis.par.get("add.text") # something better ?
                tmp <- list(label = if (is.list(labels)) labels[[1]] else labels,
                            col = text$col, rot = text$rot,
                            cex = text$cex,
                            fontfamily = text$fontfamily,
                            fontface = text$fontface,
                            font = text$font)

                labels <- updateList(tmp, if (is.list(labels)) labels else list()) # FIXME: risky

                if (!is.characterOrExpression(labels$label))
                    labels$label <- format(at)
            }

            add.line <- trellis.par.get("add.line")
            add.text <- trellis.par.get("add.text")

            ## convert z into a matrix, with NA entries for those
            ## 'missing' from data frame. There's scope for ambiguity
            ## here, which can be avoided by the user.

            m <- matrix(NA, nrow = length(ux), ncol = length(uy))
            m[(idy - 1) * length(ux) + idx ] <- z

            clines <-
                contourLines(x = ux, y = uy, z = m,
                             nlevels = length(at), ## necessary ?
                             levels = at)

            for (val in clines) {

                ## each val looks like:

                ## $ :List of 3
                ##  ..$ level: num 170
                ##  ..$ x    : num [1:21] 0.535 0.534 0.534 0.534 0.535 ...
                ##  ..$ y    : num [1:21] 0.398 0.400 0.417 0.433 0.434 ...

                ## we don't know how to leave gap in lines for labels.

                llines(val, ## hopefully $levels won't matter
                       col = col, lty = lty, lwd = lwd)


                ## if too small, don't add label. How small is small ?
                ## Should depend on resolution. How ?

                if (length(val$x) > 5)
                {
                    if (!is.null(labels))
                    {
                        slopes <- diff(val$y) / diff(val$x)
                        ## slopes[is.na(slopes)] <- 0

                        if (label.style == "flat")
                        {
                            ## draw label at 'flattest' position along contour

                            textloc <- which.min(abs(slopes))
                            rotangle <- 0
                        }
                        else if (label.style == "align")
                        {

                            ## draw label at 'deepest' position along
                            ## contour, depth being min distance to either
                            ## of the four edges, scaled appropriately

                            rx <- range(ux)
                            ry <- range(uy)
                            depth <- pmin(pmin(val$x - rx[1], rx[2] - val$x) / diff(rx), 
                                          pmin(val$y - ry[1], ry[2] - val$y) / diff(ry))
                            textloc <- min(which.max(depth), length(slopes)) 
                                        # slopes has one less entry,
                                        # and textloc indexes slopes

                            rotangle <- atan(slopes[textloc] * diff(rx) / diff(ry)) * 180 / base::pi
                        }
                        else if (label.style == "mixed")
                        {

                            ## mix both. align for contours whose flattest
                            ## portion is too close to edge

                            rx <- range(ux)
                            ry <- range(uy)
                            depth <- pmin(pmin(val$x - rx[1], rx[2] - val$x) / diff(rx), 
                                          pmin(val$y - ry[1], ry[2] - val$y) / diff(ry))
                            textloc <- which.min(abs(slopes))
                            rotangle <- 0


                            if (depth[textloc] < .05 ) {
                                textloc <- min(which.max(depth), length(slopes))
                                rotangle <- atan(slopes[textloc] * diff(rx) / diff(ry)) * 180 / base::pi
                            }

                        }
                        else stop("Invalid label.style")

                        i <- match(val$level, at)

                        
                        ltext(lab = labels$lab[i], adj = c(.5, 0),
                              srt = rotangle,
                              col = labels$col,
                              cex = labels$cex,
                              font = labels$font,
                              fontfamily = labels$fontfamily,
                              fontface = labels$fontface,
                              x = .5 * (val$x[textloc]+val$x[textloc + 1]),
                              y = .5 * (val$y[textloc]+val$y[textloc + 1]))

                    }
                }
            }
        }
    }
}









contourplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.contourplot",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 7,
             labels = TRUE,
             contour = TRUE,
             pretty = TRUE,
             region = FALSE,
             ...,
             subset = TRUE)

{
    ## m <- match.call(expand.dots = FALSE)
    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    do.call("levelplot",
            c(list(formula = substitute(formula),
                   data = data,
                   groups = groups,
                   subset = subset,
                   panel = panel,
                   prepanel = prepanel,
                   strip = strip,
                   labels = labels,
                   cuts = cuts,
                   contour = contour,
                   pretty = pretty,
                   region = region),
              dots))
}





levelplot <-
    function(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = TRUE,
             aspect = "fill",
             panel = "panel.levelplot",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,

             ## at, region etc should be ideally in panel.levelplot only, but is needed for colorkey
             at,
             cuts = 15,
             pretty = FALSE,
             region = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(),
             colorkey = region,
             col.regions,
             alpha.regions,
             subset = TRUE)
{

    ##dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, z etc. and do some preprocessing

    left.name <- deparse(substitute(formula))
    formula <- eval(substitute(formula), data, parent.frame())
    form <-
        if (inherits(formula, "formula"))
            latticeParseFormula(formula, data, dim = 3,
                                subset = subset, groups = groups,
                                multiple = allow.multiple,
                                outer = outer, subscripts = TRUE,
                                drop = drop.unused.levels)
        else {
            if (is.matrix(formula)) {
                tmp <- expand.grid(1:nrow(formula), 1:ncol(formula))
                list(left = as.vector(formula),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     left.name = left.name,
                     right.x.name = "row", right.y.name = "column",
                     subscr = seq(length = nrow(tmp)))
            }
            else if (is.data.frame(formula)) {
                tmp <- expand.grid(rownames(formula), colnames(formula))
                list(left = as.vector(as.matrix(formula)),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     left.name = left.name,
                     right.x.name = "row", right.y.name = "column",
                     subscr = seq(length = nrow(tmp)))
            }
            else stop("invalid formula")
        }


    ## We need to be careful with 'subscripts' here. It HAS to be
    ## there, and it's to be used to index x, y, z (and not only
    ## groups, unlike in xyplot etc). This means we have to subset
    ## groups as well, which is about the only use for the subscripts
    ## calculated in latticeParseFormula, after which subscripts is
    ## regenerated as a straight sequence indexing the variables

    if (!is.null(form$groups))
        groups <-
            if (is.matrix(form$groups)) as.vector(form$groups)[form$subscr]
            else if (is.data.frame(form$groups)) as.vector(as.matrix(form$groups))[form$subscr]
            else form$groups[form$subscr]

    subscr <- seq(length = length(form$left))

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    cond <- form$condition
    number.of.cond <- length(cond)
    z <- form$left
    x <- form$right.x
    y <- form$right.y

    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        number.of.cond <- 1
    }

    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name

    zrng <- extend.limits(range(z[!is.na(z)]))
    if (missing(at))
        at <-
            if (pretty) pretty(zrng, cuts)
            else seq(zrng[1], zrng[2], length = cuts+2)
    
    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(cond = cond,
                          aspect = aspect,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab,
                          xlab.default = form$right.x.name,
                          ylab.default = form$right.y.name), dots))

    
    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (excluding limits)

    ## scales <- eval(substitute(scales), data, parent.frame())
    if (is.character (scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo,
             do.call("construct.scales", scales))


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

    id.na <- is.na(x) | is.na(y)  ##|is.na(z)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    ## Most levelplot/contourplot specific code here




    if (is.logical(colorkey))
    {
        if (colorkey)
        {
            colorkey <- list(at = at, space = "right")
            if (!missing(col.regions)) colorkey$col <- col.regions
            if (!missing(alpha.regions)) colorkey$alpha <- alpha.regions
        }
        else colorkey <- NULL
    }
    else if (is.list(colorkey))
    {
        tmp <- ## FIXME: does the inside thing work? probably not 
            list(space = if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right",
                 at = at)
        if (!missing(col.regions)) tmp$col <- col.regions
        if (!missing(alpha.regions)) tmp$alpha <- alpha.regions
        colorkey <- updateList(tmp, colorkey)
    }
    foo$legend <-
        construct.legend(foo$legend,
                         colorkey,
                         fun = "draw.colorkey")

    foo$panel.args.common <-
        c(list(x = x, y = y, z = z, at = at,
               region = region), dots)
    if (!missing(col.regions)) foo$panel.args.common$col.regions <- col.regions
    if (!missing(alpha.regions)) foo$panel.args.common$alpha.regions <- alpha.regions









# ############### premature calculation of col.regions
#     ## region
#     numcol <- length(at)-1
#     numcol.r <- length(col.regions)

#     col.regions <-
#         if (numcol.r <= numcol)
#             rep(col.regions, length = numcol)
#         else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]

#     if (is.logical(colorkey))
#     {
#         if (colorkey) colorkey <-
#             list(space = "right", col = col.regions,
#                  at = at, tick.number = 7)
#         else colorkey <- NULL
#     }
#     else if (is.list(colorkey))
#     {
#         #foo$colorkey <- colorkey
#         if (is.null(colorkey$col)) colorkey$col <- col.regions
#         if (is.null(colorkey$at)) colorkey$at <- at
#         if (is.null(colorkey$space)) colorkey$space <-
#             if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right"
#     }
#     foo$legend <-
#         construct.legend(foo$legend,
#                          colorkey,
#                          fun = "draw.colorkey")

#     zcol <- rep(NA, length(z)) #numeric(length(z))
#     for (i in seq(along=col.regions))
#         zcol[!id.na & !is.na(z) & z>=at[i] & z<at[i+1]] <- i

#     foo$panel.args.common <-
#         c(list(x=x, y=y, z=z, at=at,
#                labels=labels,
#                region = region, contour = contour,
#                zcol=zcol,
#                col.regions=col.regions),
#           dots)
# ##############################




    if (!is.null(groups)) foo$panel.args.common$groups <- groups

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

    more.comp <- c(limits.and.aspect(prepanel.default.levelplot,
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








