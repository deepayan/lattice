### Copyright 2001-2003  Deepayan Sarkar <deepayan@stat.wisc.edu>
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




construct.scales <-
    function(draw = TRUE,
             axs = "r",
             tck = 1,
             tick.number = 5,
             lty = FALSE, lwd = FALSE,
             cex = FALSE,
             rot = FALSE,
             at = FALSE,
             labels = FALSE,
             col = FALSE,
             col.line = col,
             log = FALSE,
             font = FALSE,
             fontfamily = FALSE,
             fontface = FALSE,
             alternating = TRUE,
             relation = "same",
             abbreviate = FALSE,
             minlength = 4,
             limits = NULL,
             format = NULL,
             x = NULL,
             y = NULL,
             ...)   ## FIXME: how to handle ...
{
    xfoo <- list(draw = draw, axs = axs, tck = tck,
                 tick.number = tick.number,
                 lty = lty, lwd = lwd,
                 cex = cex,
                 rot = rot,
                 font = font,
                 fontfamily = fontfamily, fontface = fontface, 
                 at = at, labels = labels,
                 col = col, col.line = col.line,
                 log = log,
                 alternating = alternating,
                 relation = relation,
                 abbreviate = abbreviate,
                 minlength = minlength,
                 limits = limits,
                 format = format)
    yfoo <- xfoo
    if (!is.null(x)) {
        if (is.character(x)) x <- list(relation = x)
        xfoo[names(x)] <- x
    }
    if (is.logical(xfoo$alternating))
        xfoo$alternating <-
            if (xfoo$alternating) c(1,2)
            else 1
    if (!is.null(y)) {
        if (is.character(y)) y <- list(relation = y)
        yfoo[names(y)] <- y
    }
    if (is.logical(yfoo$alternating))
        yfoo$alternating <-
            if (yfoo$alternating) c(1,2)
            else 1
    for (nm in c("tck", "cex", "rot")) {
        xfoo[[nm]] <- rep(xfoo[[nm]], length = 2)
        yfoo[[nm]] <- rep(yfoo[[nm]], length = 2)
    }
    if (xfoo$rel == "same" && (is.list(xfoo$at) || is.list(xfoo$lab)))
        stop("the at and labels components of scales may not be lists when relation = same")
    if (yfoo$rel == "same" && (is.list(yfoo$at) || is.list(yfoo$lab)))
        stop("the at and labels components of scales may not be lists when relation = same")
    list(x.scales = xfoo, y.scales = yfoo)
}




construct.3d.scales <-
    function(draw = TRUE,
             axs = "r",
             tck = 1,
             lty = FALSE, lwd = FALSE,
             distance = c(1,1,1),
             tick.number = 5,
             cex = FALSE,
             rot = FALSE,
             at = FALSE,
             labels = FALSE,
             col = FALSE,
             col.line = col,
             log = FALSE,
             font = FALSE,
             fontfamily = FALSE,
             fontface = FALSE,
             arrows = TRUE,
             relation = "same",
             format = NULL,
             abbreviate = FALSE,
             minlength = 4,
             x = NULL,
             y = NULL,
             z = NULL,
             ...)
{
    xfoo <- list(draw = draw, axs = axs, tck = tck,
                 lty = lty, lwd = lwd,
                 tick.number = tick.number,
                 cex = cex, rot = rot, font = font,
                 fontfamily = fontfamily, fontface = fontface, 
                 at = at, labels = labels,
                 col = col, col.line = col.line,
                 log = log, arrows = arrows,
                 relation = relation, format = format,
                 abbreviate = abbreviate, minlength = minlength)
    yfoo <- xfoo
    zfoo <- xfoo
    distance <- rep(distance, length = 3)
    xfoo$distance <- distance[1]
    yfoo$distance <- distance[2]
    zfoo$distance <- distance[3]
    if (!is.null(x)) {
        if (is.character(x)) x <- list(relation = x)
        xfoo[names(x)] <- x
    }
    if (!is.null(y)) {
        if (is.character(y)) y <- list(relation = y)
        yfoo[names(y)] <- y
    }
    if (!is.null(z)) {
        if (is.character(z)) z <- list(relation = z)
        zfoo[names(z)] <- z
    }
    list(x.scales = xfoo, y.scales = yfoo, z.scales = zfoo)
}












limitsFromLimitlist <- function(have.lim, lim, relation, limitlist, axs, nplots)
    ## have.lim: logical, whether xlim/ylim was explicitly specified
    ## lim: the specified limit if have.lim = TRUE
    ## relation: same/free/sliced
    ## limitlist: list of limits from prepanel calculations, one for each panel
    ## axs: "r", "i" etc, passed on to extend.limits

    ## return value depends on relation. (See limits.and.aspect below
    ## where this is used for partial enlightenment.)

{

    if (relation == "same") {

        ## The problem here is that we need to figure out the overall
        ## limit required from the limits of each panel. This could be
        ## a problem for two reasons. First, some panels could have no
        ## data in them, in which case the corresponding limits would
        ## be NA. Secondly, the limits could be either numeric or
        ## character vectors (the latter for factors). When relation =
        ## same, the type should be same across panels. When numeric,
        ## we just take range, leaving out NAs. But what about
        ## factors?  Is it OK to assume that all the non-NA vectors
        ## would be exactly the same ? They should be, since levels(x)
        ## would not change even if not all levels are
        ## represented. So, I'm just taking unique of all the vectors
        ## concatenated, excluding NA's

        ## Additional complication: Need to preserve class of limits,
        ## to be used later in tick location/label calculation. Not a
        ## problem in other cases, but here unlist-ing loses the
        ## class.


        if (have.lim) {
            if (is.list(lim)) stop("limits cannot be a list when relation = same")
            limits <- lim
            slicelen <- if (is.numeric(lim)) diff(range(lim)) else length(lim) + 2
        }
        else {

            ## should check that all classes are the same. How ? What about NA's ? Arrgh!
            ## to handle NA's, how about:

            all.na <- unlist(lapply(limitlist, function(x) all(is.na(x))))
            class.lim <- lapply(limitlist[!all.na],  ## retain non-NA limitlists only
                                class)
            ## class.lim is a list now, may be length 0

            limits <- unlist(limitlist) ## loses the class attribute

            if (length(limits) > 0) {

                if (is.numeric(limits)) {

                    limits <- extend.limits(range(limits, na.rm = TRUE), axs = axs)
                    slicelen <- diff(range(limits))

                }
                else {
                    limits <- unique(limits[!is.na(limits)])
                    slicelen <- length(limits) + 2
                }

                ## hopefully put back appropriate class of limits:

                if (length(class.lim) > 0) class(limits) <-
                    if (all(class.lim[[1]] == "integer")) "numeric" else class.lim[[1]]

                ## (have to handle "integer" specially, since variable
                ## specifications like 1:10 are rather common, and
                ## class() <- "integer" would turn the limits into
                ## integers)

            }
            else {
                limits <- c(0,1)
                slicelen <- 1
            }

        }

        ans <- list(limits = limits, slicelen = slicelen)
    }


    else if (relation == "sliced") {

        if (have.lim) {
            if (is.list(lim)) {
                limits <- rep(lim, length = nplots)
            }
            else warning("Explicitly specified limits ignored")
        }
        slicelen <- limitlist
        for (i in seq(along = limitlist))
            slicelen[[i]] <-
                if (is.numeric(limitlist[[i]]))
                    diff(range(limitlist[[i]])) # range unnecessary, but...
                else NA
        slicelen <- (if (axs == "i") 1 else 1.14) * max(unlist(slicelen), na.rm = TRUE)
        for (i in seq(along = limitlist)) {
            if (is.numeric(limitlist[[i]]))
                limitlist[[i]] <-
                    extend.limits(limitlist[[i]], length = slicelen)
        }
        ans <- list(limits = limitlist, slicelen = slicelen)
    }


    else if (relation == "free") {

        if (have.lim) {

            ## FIXME: What's all this about ? I seem to have forgotten
            ## :-( what should happen if xlim is specified ? Should
            ## that override "free" ? Make sure docs are consistent

            if (!is.list(lim)) lim <- list(lim)

            id <- logical(length(limitlist))
            for (i in seq(along = id)) 
                id[i] <- !any(is.na(limitlist[[i]]))
            id <- seq(along = id)[id]
            id <- id[!is.na(id)]  ## how can this be of any use ?

            limitlist[id] <- lim
        }

        for (i in seq(along = limitlist)) {
            if (is.numeric(limitlist[[i]])) 
                limitlist[[i]] <- extend.limits(limitlist[[i]], axs = axs) ## preserves class
            ## o.w., keep it as it is
        }
        ans <- list(limits = limitlist, slicelen = 1)
    }

    ans

}

















limits.and.aspect <-
    function(prepanel.default.function,
             prepanel = NULL,
             have.xlim = FALSE, xlim = NULL,
             have.ylim = FALSE, ylim = NULL,
             x.relation, y.relation,
             panel.args.common = list(),
             panel.args = list(),
             aspect,
             nplots = length(panel.args),
             x.axs = "r", y.axs = "r",
             ...)  ## extra arguments for prepanel (for qqmathline)
{

    if (nplots<1) stop("need at least one panel")
    x.limits <- vector("list", nplots)
    y.limits <- vector("list", nplots)
    dxdy <- vector("list", nplots)

    for (count in seq(length = nplots))
    {
        if (is.list(panel.args[[count]]))
        {
            pargs <- c(panel.args.common, panel.args[[count]], list(...))
            tem <- do.call("prepanel.default.function", pargs)
            if (is.function(prepanel)) ## results will 'overwrite' defaults
            {
                prenames <- names(formals(prepanel))
                if (!("..." %in% prenames)) pargs <- pargs[prenames]
                pretem <- do.call("prepanel", pargs)
                tem[names(pretem)] <- pretem
            }
            x.limits[[count]] <- tem$xlim
            y.limits[[count]] <- tem$ylim
            dxdy[[count]] <- list(tem$dx, tem$dy)
        }
        else  ## this happens for empty panels
        {
            x.limits[[count]] <- c(NA, NA)
            y.limits[[count]] <- c(NA, NA)
            dxdy[[count]] <- list(NA, NA)
        }
    }

    ## Some explanation might be helpful here. The for loop above
    ## creates a list of xlims/ylims. Each of these might be either
    ## numeric (when x/y is numeric, shingle or POSIXt etc), or levels
    ## of a factor (that's how prepanel.default.functions are set
    ## up). However, at this point, all x.limits[[i]] must be of the
    ## same type. Returned limits must be in accordance with this
    ## type. The only exception is when relation = "free", in which
    ## case they may be different. This could happen if [xy]lim or
    ## limits is supplied as a list in the high level function.

    x.limits <-
        limitsFromLimitlist(have.lim = have.xlim,
                            lim = xlim,
                            relation = x.relation,
                            limitlist = x.limits,
                            axs = x.axs,
                            nplots = nplots)


    y.limits <-
        limitsFromLimitlist(have.lim = have.ylim,
                            lim = ylim,
                            relation = y.relation,
                            limitlist = y.limits,
                            axs = y.axs,
                            nplots = nplots)

    if (is.character(aspect))

        if (aspect == "xy") {

            aspect <- median(unlist(lapply(dxdy, banking)),
                             na.rm = TRUE) * y.limits$slicelen /
                                 x.limits$slicelen

            if (y.relation == "free")
                warning("aspect=xy when y-relation=free is not sensible")

        }
        else aspect <- 1

    list(x.limits = x.limits$limits,
         y.limits = y.limits$limits,
         aspect.ratio = aspect,
         prepanel = prepanel)
}


