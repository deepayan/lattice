


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



## retrieve last saved (while printing) trellis object

trellis.last.object <- function(warn = TRUE, ...)
{
    ans <- get("last.object", envir = .LatticeEnv)
    if (is.null(ans)) {
        warning("No trellis object currently saved")
        return(invisible())
    }
    if (warn && !lattice.getStatus("current.plot.saved"))
        warning("currently saved object is not the last one plotted")
    update(ans, ...)
}


 
## Not all arguments to xyplot etc can be supplied to
## update.trellis. Generally speaking, anything that needs to change
## the data within each panel is a no-no. Everything else is
## technically game, though implementation might be
## problematic. Here's a list of arguments that should work (list
## currently based on xyplot, may need to be updated later)


##             panel
##             aspect = "fill",
##             as.table = FALSE,
##             between,
##             key,
##             auto.key = FALSE,
##             layout,
##             main,
##             page,
##             par.strip.text,
##             prepanel,

## scales, one of the problematic ones

##             skip,
##             strip,
##             sub,
##             xlab,
##             xlim,
##             ylab,
##             ylim,
##             par.settings,



## ...,  these should probably be added to the list of common panel arguments



## There is also the possibility of some update arguments that may not
## necessarily be valid arguments to xyplot etc (although we might
## change that). Currently these would be the perm and index arguments
## controlling reordering of conditioning variables and their levels.




update.trellis <-
    function(object,
             panel,
             aspect,
             as.table,
             between,
             key,
             auto.key,
             legend,
             layout,
             main,
             page,
             par.strip.text,
             prepanel,
             scales, #one of the problematic ones
             skip,
             strip,
             sub,
             xlab,
             xlim,
             ylab,
             ylim,
             par.settings,

             index.cond,
             perm.cond,

             ...)

{
    ## modify call to reflect update
    upcall <- match.call()
    nm <- names(upcall)
    if (!is.null(nm))
    {
        nm <- nm[nm != "" & nm != "object"]
        if (length(nm) == 0)
        {
            ## FIXME: drop this message before release
            ## cat("nothing to update with")
            return(object)
        }
        object$call[nm] <- upcall[nm]
    }


    have.xlim <- !missing(xlim)    ## needed later
    have.ylim <- !missing(xlim)


    ## deal with the non-problematic stuff first

    if (!missing(as.table))
    {
        if (is.logical(as.table)) object$as.table <- as.table
        else warning("Inappropriate value of as.table")
    }
    
    if (!missing(between))
    {
        if ("x" %in% names(between)) object$x.between <- between$x
        if ("y" %in% names(between)) object$y.between <- between$y
    }

    if (!missing(layout))
    {
        object$layout <- layout
    }

    if (!missing(main)) object$main <- main
    if (!missing(sub)) object$sub <- sub
    if (!missing(xlab)) object$xlab <- xlab
    if (!missing(ylab)) object$ylab <- ylab


    if (!missing(page))
    {
        object$page <- page
    }

    if (!missing(par.strip.text))
    {
        ## this only overwrites earlier things, leaves alone those
        ## that are not specified explicitly

        if (is.list(par.strip.text))
            object$par.strip.text <- updateList(object$par.strip.text, par.strip.text)
        else warning("par.strip.text must be a list")
    }

    if (!missing(skip))
    {
        object$skip <- skip
    }

    if (!missing(strip))
    {
        if (is.logical(strip)) {
            if (strip) object$strip <- strip.default
            else object$strip <- FALSE
        }
        else object$strip <- strip
    }

    if (!missing(par.settings))
    {
        ## this only overwrites earlier things, leaves alone those
        ## that are not specified explicitly

        if (is.list(par.settings))
            object$par.settings <- updateList(object$par.settings, par.settings)
        else warning("par.settings must be a list")
    }


    ## during construction of trellis objects, perm.cond and
    ## index.cond are calculated by the cond.orders function. We could
    ## do that here as well, but the perm.cond is really too trivial
    ## to bother. cond.orders() is called is index.cond is
    ## non-missing, and then it becomes important that perm.cond is
    ## processed first (in case it it non-missing as well).

    if (!missing(perm.cond))
    {
        if (is.null(perm.cond))
            object$perm.cond <- seq(length = length(object$condlevels))
        else if (all(sort(perm.cond) == object$perm.cond))
            object$perm.cond <- perm.cond
        else stop("Invalid value of perm.cond")
    }

    if (!missing(index.cond))
    {
        object$index.cond <- index.cond
        cond.ord <- cond.orders(object)
        object[names(cond.ord)] <- cond.ord
    }

    dots <- list(...)
    if (length(dots) > 0)
    {
        ##print(dots) ## for debugging, remove later
        object$panel.args.common <- updateList(object$panel.args.common, dots)
    }



    if (!missing(panel))
    {
        panel <- 
            if (is.function(panel)) panel 
            else if (is.character(panel)) get(panel)
            else eval(panel)

        if (as.character(object$call[[1]]) == "splom")
            object$panel.args.common$panel <- panel
        else object$panel <- panel
    }





    ## the slightly complicated stuff


    if (!missing(legend))
    {
        if (is.null(legend)) object$legend <- NULL
        else object$legend <- updateList(object$legend, legend)
    }

    

    
    if (!missing(key))  ## FIXME: why?
    {
        ## should we allow partial update?
        ## object$key <- updateList(object$key, key)
        object$key <- key
    }



    if (!missing(auto.key))
    {
        if (!is.null(legend))
            cat(paste("\nNote: auto.key ignored since key already present. \n",
                      "Use update(..., legend = NULL) to remove exisitng legend(s)\n"))
        else 
        {
            groups <- object$panel.args.common$groups

            if (!is.null(groups) && (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
            {
                object$legend <-
                    list(list(fun = "drawSimpleKey",
                              args =
                              updateList(list(text = levels(as.factor(groups))), 
                                         if (is.list(auto.key)) auto.key else list())))
                object$legend[[1]]$x <- object$legend[[1]]$args$x
                object$legend[[1]]$y <- object$legend[[1]]$args$y
                object$legend[[1]]$corner <- object$legend[[1]]$args$corner

                names(object$legend) <- 
                    if (any(c("x", "y", "corner") %in% names(object$legend[[1]]$args)))
                        "inside"
                    else
                        "top"
                if (!is.null(object$legend[[1]]$args$space))
                    names(object$legend) <- object$legend[[1]]$args$space
            }
        }
    }


    relationChanged <- FALSE

    if (!missing(scales))
        ## FIXME: this needs special handling for cloud, but leave that for later
    {
        if (is.character(scales)) scales <- list(relation = scales)
        xscales <- scales$x
        yscales <- scales$y
        zscales <- scales$z
        scales$x <- NULL
        scales$y <- NULL
        scales$z <- NULL
        if (is.character(xscales)) xscales <- list(relation = xscales)
        if (is.character(yscales)) yscales <- list(relation = yscales)
        if (is.character(zscales)) zscales <- list(relation = zscales)

        if (!is.null(scales$log) || !is.null(xscales$log) || !is.null(yscales$log) || !is.null(zscales$log))
        {
            warning("log scales cannot be changed via update")
            scales$log <- NULL
            xscales$log <- NULL
            yscales$log <- NULL
            zscales$log <- NULL
        }

        if (is.logical(scales$alternating)) scales$alternating <- if (scales$alternating) c(1,2) else 1
        if (is.logical(xscales$alternating)) xscales$alternating <- if (xscales$alternating) c(1,2) else 1
        if (is.logical(yscales$alternating)) yscales$alternating <- if (yscales$alternating) c(1,2) else 1
        ## cannot possibly make sense for z

        for (nm in c("tck", "cex", "rot"))
        {
            scales[[nm]] <- rep(scales[[nm]], length = 2)
            xscales[[nm]] <- rep(xscales[[nm]], length = 2)
            yscales[[nm]] <- rep(yscales[[nm]], length = 2)
            zscales[[nm]] <- rep(zscales[[nm]], length = 2)
        }

        if (!is.null(scales$limits))
        {
            have.xlim <- TRUE
            have.ylim <- TRUE
            ##have.zlim <- TRUE
            xlim <- scales$limits
            ylim <- scales$limits
            ##zlim <- scales$limits
        }

        if (!is.null(xscales$limits))
        {
            have.xlim <- TRUE
            xlim <- scales$limits
        }
        if (!is.null(yscales$limits))
        {
            have.ylim <- TRUE
            xlim <- scales$limits
        }

        if (!is.null(scales$relation) || !is.null(xscales$relation) || !is.null(yscales$relation))
            relationChanged <- TRUE

        object$x.scales[names(scales)] <- scales
        object$y.scales[names(scales)] <- scales
        object$z.scales[names(scales)] <- scales

        object$x.scales[names(xscales)] <- xscales
        object$y.scales[names(yscales)] <- yscales
        object$z.scales[names(zscales)] <- zscales

        if (object$x.scales$relation == "same" && (is.list(object$x.scales$at) || is.list(object$x.scales$lab)))
            stop("the at and labels components of scales may not be lists when relation = same")
        if (object$y.scales$relation == "same" && (is.list(object$y.scales$at) || is.list(object$y.scales$lab)))
            stop("the at and labels components of scales may not be lists when relation = same")
    }







    ## difficult stuff

#             aspect
#             prepanel,
#             scales, #one of the problematic ones
#             xlim,
#             ylim,




    ## stuff that may need recalculation of limits and aspect ratio

    recalculateLimits <- have.xlim || have.ylim || relationChanged

    if (!missing(aspect))
    {
        if (is.numeric(aspect))
        {
            object$aspect.ratio <- aspect
            object$aspect.fill <- FALSE
        }
        else if (is.character(aspect))
        {
            if (aspect == "fill") object$aspect.fill <- TRUE
            else if (aspect == "xy")
            {
                object$aspect.fill <- FALSE
                object$aspect.ratio <- "xy" ## guaranteed to be modified below
                recalculateLimits <- TRUE
            }
            else if (aspect == "iso")
            {
                object$aspect.fill <- FALSE
                object$aspect.ratio <- "iso" ## guaranteed to be modified below
                recalculateLimits <- TRUE
            }
            else warning(paste("Unrecognized value of aspect:", aspect))
        }
        else warning("Invalid value of aspect")
    }





    if (!missing(prepanel))
    {
        recalculateLimits <- TRUE

        prepanel <-
            if (is.function(prepanel)) prepanel 
            else if (is.character(prepanel)) get(prepanel)
            else eval(prepanel)
    }
    else prepanel <- object$prepanel


    if (recalculateLimits)
    {
        prepanel.def <- get(paste("prepanel.default", object$call[[1]], sep = "."))
        laa <- limits.and.aspect(prepanel.default.function = prepanel.def,
                                 prepanel = prepanel,
                                 have.xlim = have.xlim,
                                 xlim = xlim,
                                 have.ylim = have.ylim,
                                 ylim = ylim,
                                 x.relation = object$x.scales$relation,
                                 y.relation = object$y.scales$relation,
                                 panel.args.common = object$panel.args.common,
                                 panel.args = object$panel.args,
                                 aspect = object$aspect.ratio)
        ##...)  ## extra arguments for prepanel (for qqmathline)

        object[names(laa)] <- laa


    }
    
    object
}



## FIXME: how to do this?
## `subsetting': shortcut to updating index.cond

## "[.trellis" <- function(x, i, j = NULL, ...)
## {
##     index.cond <- list(i, 
## }






