


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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA



## accessors for a grid layouts nrow and ncol

layoutNRow <- function(x) x$nrow
layoutNCol <- function(x) x$ncol

## other accessors, for during (and sometimes after) plotting

current.row <- function() lattice.getStatus("current.focus.row")
current.column <- function() lattice.getStatus("current.focus.column")
trellis.currentLayout <- function(which = c("packet", "panel"))
{
    which <- match.arg(which)
    switch(which,
           packet = lattice.getStatus("current.packet.positions"),
           panel = lattice.getStatus("current.panel.positions"))
}
panel.number <- function() trellis.currentLayout("panel")[current.row(), current.column()]
packet.number <- function() trellis.currentLayout("packet")[current.row(), current.column()]
which.packet <- function() lattice.getStatus("current.cond.levels")[[current.row(), current.column()]]


## utility to create a full-fledged list describing a label from parts
## (used for main, sub, xlab, ylab)

getLabelList <- function(label, text.settings, default.label = NULL)
{
    if (!is.null(label))
    {
        if (inherits(label, "grob")) return(label)
        ans <-
            list(label = 
                 if (is.characterOrExpression(label)) label
                 else if (is.list(label) && (is.null(names(label)) || names(label)[1] == "")) label[[1]]
                 else default.label,
                 col = text.settings$col, cex = text.settings$cex,
                 fontfamily = text.settings$fontfamily,
                 fontface = text.settings$fontface,
                 font = text.settings$font,
                 lineheight = text.settings$lineheight)
        if (is.list(label)) ans[names(label)] <- label
    }
    else ans <- NULL
    if (is.null(ans$lab) ||
        (is.character(ans) && ans$lab == "")) ans <- NULL
    ans
}


drawInViewport <-
    function(obj, vp)
{
    pushViewport(vp)
    grid.draw(obj)
    upViewport()
}





grobFromLabelList <- function(lab, name = "label", rot = 0)
{
    if (is.null(lab) || (is.character(lab) && lab == "")) return (NULL)
    if (inherits(lab, "grob")) return(lab)
    textGrob(label = lab$label, name= name, rot = rot,
             gp =
             gpar(col = lab$col,
                  fontfamily = lab$fontfamily,
                  fontface = chooseFace(lab$fontface, lab$font),
                  lineheight = lab$lineheight,
                  alpha = lab$alpha,
                  cex = lab$cex))
}






evaluate.legend <- function(legend)
{
    if (is.null(legend)) return(NULL)
    for (i in seq_along(legend))
    {
        fun <- legend[[i]]$fun
        fun <- 
            if (is.function(fun)) fun 
            else if (is.character(fun)) get(fun)
            else eval(fun)  ## in case fun is a grob (is this OK?)
        if (is.function(fun)) fun <- do.call("fun", legend[[i]]$args)
        legend[[i]]$obj <- fun
        legend[[i]]$args <- NULL
        legend[[i]]$fun <- NULL
    }
    legend
}








## S3 plot method for "trellis" objects (essentially an alias to
## print.trellis)

plot.trellis <-
    function(x, ...)
{
    print(x, ...)
    invisible()
}







## S3 print method for "trellis" objects

print.trellis <-
    function(x, position, split, more = FALSE,
             newpage = TRUE,
             packet.panel = packet.panel.default,
             draw.in = NULL,
             panel.height = lattice.getOption("layout.heights")$panel,
             panel.width = lattice.getOption("layout.widths")$panel,
             save.object = lattice.getOption("save.object"),
             prefix,
             ...)
{

    ## save the current object, if so requested.  This used to be done
    ## at the end, so that it wouldn't happen if there were errors
    ## during printing.  However, doing this now will allow things
    ## like trellis.panelArgs() to work in panel/axis functions, which
    ## I think is a better trade-off.

    if (save.object)
    {
        assign("last.object", x, env = .LatticeEnv)
        lattice.setStatus(current.plot.saved = TRUE)
    }
    else
        lattice.setStatus(current.plot.saved = FALSE)


    ## make sure we have a device open

    if (is.null(dev.list())) trellis.device()
    else if (is.null(trellis.par.get()))
        trellis.device(device = .Device, new = FALSE)


    ## if necessary, save current settings and apply temporary
    ## settings in x$par.settings

    ## FIXME: do these using on.exit rather than the current scheme

    if (!is.null(x$par.settings))
    {
        ## save current state, restore later
        opar <- trellis.par.get() ## get("lattice.theme", envir = .LatticeEnv)
        trellis.par.set(theme = x$par.settings)
    }

    ## do the same for lattice.options

    if (!is.null(x$lattice.options))
    {
        ## save current state, restore later
        oopt <- lattice.options(x$lattice.options)
    }


    ## We'll also allow arguments to print.trellis (or plot.trellis)
    ## to be included within a trellis object.

    if (!is.null(x$plot.args))
    {
        ## can't think of a clean way, so...
        renewArg <- function(name) if (is.null(x$plot.args[[name]])) get(name) else x$plot.args[[name]]
        for (nm in c("position", "split", "more", "newpage",
                     "packet.panel", "draw.in", "panel.height",
                     "panel.width", "save.object", "prefix"))
        {
            assign(nm, renewArg(nm))
        }

    }



    bg <- trellis.par.get("background")$col
    new <-  newpage && !lattice.getStatus("print.more") && is.null(draw.in)
    if (!is.null(draw.in)) depth <- downViewport(draw.in)

    lattice.setStatus(print.more = more)
    usual  <- (missing(position) & missing(split))
    ##if (!new && usual)
    ##    warning("more is relevant only when split/position is specified")


    ## this means this plot will be the first one on a new page
    if (new) lattice.setStatus(plot.index = 1)

    ## get default prefix for grid viewport/object names
    if (missing(prefix))
        prefix <- paste("plot", lattice.getStatus("plot.index"), sep = "")
    lattice.setStatus(current.prefix = prefix)
    lattice.setStatus(plot.index = 1 + lattice.getStatus("plot.index"))

    global.gpar <-
        do.call(gpar,
                updateList(trellis.par.get("grid.pars"),
                           list(fontsize = trellis.par.get("fontsize")$text)))

    if (!missing(position))
    {
        stopifnot (length(position) == 4)
        if (new)
        {
            grid.newpage()
            grid.rect(gp = gpar(fill = bg, col = "transparent"))
        }
        pushViewport(viewport(x = position[1], y = position[2],
                              width = position[3] - position[1],
                              height = position[4] - position[2],
                              just = c("left","bottom"),
                              name = trellis.vpname("position")))

        if (!missing(split))
        {
            stopifnot (length(split) == 4)
            pushViewport(viewport(layout = grid.layout(nrow = split[4], ncol = split[3]),
                                  name = trellis.vpname("split") ))
            pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1],
                                  name = trellis.vpname("split.location") ))
        }
    }


    else if (!missing(split))
    {
        stopifnot(length(split) == 4)
        if (new)
        {
            grid.newpage()
            grid.rect(gp = gpar(fill = bg, col = "transparent"))
        }
        pushViewport(viewport(layout = grid.layout(nrow = split[4], ncol = split[3]),
                              name = trellis.vpname("split") ))
        pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1],
                              name = trellis.vpname("split.location") ))
    }


    ## order.cond will be a multidimensional array, with
    ## length(dim(order.cond)) = number of conditioning
    ## variables. It's a numeric vector 1:(number.of.panels), with
    ## dim() = c(nlevels(g1), ..., nlevels(gn)), where g1, ..., gn are
    ## the conditioning variables.

    ## manipulating order.cond has 2 uses. Using normal indexing, the
    ## order of plots within a conditioning variable can be altered,
    ## or only a subset of the levels used. Also, using aperm, the
    ## order of conditioning can be altered.

    ## the information required to make the appropriate permutation
    ## and indexing is in the components index.cond and perm.cond of
    ## the trellis object


### FIXME: need some thinking here.

    ## ## Original version (up to 0.13 series):
    
    ## order.cond <- seq_len(prod(sapply(x$condlevels, length)))
    ## dim(order.cond) <- sapply(x$condlevels, length)

    ## ## first subset, then permute
    ## order.cond <- do.call("[", c(list(order.cond), x$index.cond, list(drop = FALSE)))
    ## order.cond <- aperm(order.cond, perm = x$perm.cond)


    ## ## New version:

    ## used.condlevels corresponds to the indexed and permuted object.
    ## These also have to be integer indices rather than character
    ## labels (necessary for 'packet.panel' computations).
    ## original.condlevels is required to interpret the results of
    ## packet.panel and associate them with packets in the original
    ## object (which are defined in terms of the original levels)

    original.condlevels <- 
        used.condlevels <-
            lapply(x$condlevels, function(x) seq_along(x))
    used.condlevels <- 
        mapply("[", used.condlevels, x$index.cond,
               MoreArgs = list(drop = FALSE),
               SIMPLIFY = FALSE)
    used.condlevels <- used.condlevels[x$perm.cond]
    inverse.permutation <- order(x$perm.cond) # used later

    ## an array giving packet numbers corresponding to
    ## original.condlevels.  The idea is to be able to figure out the
    ## packet given levels of the conditioning variables.  The packets
    ## are naturally thought of as an array.  The packet number simply
    ## counts positions of this array in the standard order
    ## (i.e. lower dimensions vary faster).

    adim <- sapply(original.condlevels, length)
    packet.array <- seq_len(prod(adim))
    dim(packet.array) <- adim



    ## FIXME: trying to find a way to make order.cond unnecessary may
    ## make things simpler, but that's not a very immediate concern
    ## (and not clear how difficult either).

##     order.cond <- seq_len(prod(sapply(x$condlevels, length)))
##     dim(order.cond) <- sapply(x$condlevels, length)

    ## first subset, then permute
##     order.cond <- do.call("[", c(list(order.cond), x$index.cond, list(drop = FALSE)))
##     order.cond <- aperm(order.cond, perm = x$perm.cond)




    ## order.cond will be used as indices for (exactly) the following

    ## 1. panel.args
    ## 2. x.limits
    ## 3. y.limits

    ## may need x$(index|perm).cond later for strip drawing

    ## cond.max.levels <- dim(order.cond)
    cond.max.levels <- sapply(used.condlevels, length)
    number.of.cond <- length(cond.max.levels)

    panel.layout <-
        compute.layout(x$layout, cond.max.levels, skip = x$skip)

    panel <- # shall use "panel" in do.call
        if (is.function(x$panel)) x$panel 
        else if (is.character(x$panel)) get(x$panel)
        else eval(x$panel)

    strip <- 
        if (is.function(x$strip)) x$strip 
        else if (is.character(x$strip)) get(x$strip)
        else eval(x$strip)
    strip.left <- 
        if (is.function(x$strip.left)) x$strip.left 
        else if (is.character(x$strip.left)) get(x$strip.left)
        else eval(x$strip.left)

    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")


    ## make sure aspect ratio is preserved for aspect != "fill" but
    ## this may not always be what's expected. In fact, aspect should
    ## be "fill" whenever panel.width or panel.height are non-default.

    ## panel.width <- 1
    if (!x$aspect.fill)
        panel.height[[1]] <- x$aspect.ratio * panel.width[[1]]


    ## Evaluate the legend / key grob(s): 

    legend <- evaluate.legend(x$legend)

    ## legend is now a list of `grob's along with placement info



    xaxis.lty <-
        if (is.logical(x$x.scales$lty)) axis.line$lty
        else x$x.scales$lty
    xaxis.lwd <-
        if (is.logical(x$x.scales$lwd)) axis.line$lwd
        else x$x.scales$lwd
    xaxis.col.line <-
        if (is.logical(x$x.scales$col.line)) axis.line$col
        else x$x.scales$col.line
    xaxis.col.text <-
        if (is.logical(x$x.scales$col)) axis.text$col
        else x$x.scales$col
    xaxis.alpha.line <-
        if (is.logical(x$x.scales$alpha.line)) axis.line$alpha
        else x$x.scales$alpha.line
    xaxis.alpha.text <-
        if (is.logical(x$x.scales$alpha)) axis.text$alpha
        else x$x.scales$alpha
    xaxis.font <-
        if (is.logical(x$x.scales$font)) axis.text$font
        else x$x.scales$font
    xaxis.fontface <-
        if (is.logical(x$x.scales$fontface)) axis.text$fontface
        else x$x.scales$fontface
    xaxis.fontfamily <-
        if (is.logical(x$x.scales$fontfamily)) axis.text$fontfamily
        else x$x.scales$fontfamily
    xaxis.cex <-
        if (is.logical(x$x.scales$cex)) rep(axis.text$cex, length = 2)
        else x$x.scales$cex
    xaxis.rot <-
        if (is.logical(x$x.scales$rot)) c(0, 0)
        else x$x.scales$rot



    yaxis.lty <-
        if (is.logical(x$y.scales$lty)) axis.line$lty
        else x$y.scales$lty
    yaxis.lwd <-
        if (is.logical(x$y.scales$lwd)) axis.line$lwd
        else x$y.scales$lwd
    yaxis.col.line <-
        if (is.logical(x$y.scales$col.line)) axis.line$col
        else x$y.scales$col.line
    yaxis.col.text <-
        if (is.logical(x$y.scales$col)) axis.text$col
        else x$y.scales$col
    yaxis.alpha.line <-
        if (is.logical(x$y.scales$alpha.line)) axis.line$alpha
        else x$y.scales$alpha.line
    yaxis.alpha.text <-
        if (is.logical(x$y.scales$alpha)) axis.text$alpha
        else x$y.scales$alpha
    yaxis.font <-
        if (is.logical(x$y.scales$font)) axis.text$font
        else x$y.scales$font
    yaxis.fontface <-
        if (is.logical(x$y.scales$fontface)) axis.text$fontface
        else x$y.scales$fontface
    yaxis.fontfamily <-
        if (is.logical(x$y.scales$fontfamily)) axis.text$fontfamily
        else x$y.scales$fontfamily
    yaxis.cex <-
        if (is.logical(x$y.scales$cex)) rep(axis.text$cex, length = 2)
        else x$y.scales$cex
    yaxis.rot <-
        if (!is.logical(x$y.scales$rot)) x$y.scales$rot
        else if (x$y.scales$relation != "same" && is.logical(x$y.scales$labels)) c(90, 90)
        else c(0, 0)




    strip.col.default.bg <-
        rep(trellis.par.get("strip.background")$col,
            length = number.of.cond)
    strip.col.default.fg <-
        rep(trellis.par.get("strip.shingle")$col,
            length = number.of.cond)
    strip.border <-
        lapply(trellis.par.get("strip.border"),
               function(x) rep(x, length = number.of.cond))


    ## Start layout calculations when only number of panels per page
    ## is pecified (this refers to the layout argument, not grid
    ## layouts)

    ## using device dimensions to calculate default layout:
    if (panel.layout[1] == 0) 
    {
        ddim <- par("din") 
        device.aspect <- ddim[2] / ddim[1]
        panel.aspect <- panel.height[[1]] / panel.width[[1]]

        plots.per.page <- panel.layout[2]
        m <- max (1, round(sqrt(panel.layout[2] * device.aspect / panel.aspect)))
        ## changes made to fix bug (PR#1744)
        n <- ceiling(plots.per.page/m)
        m <- ceiling(plots.per.page/n)
        panel.layout[1] <- n
        panel.layout[2] <- m

    }

    ## End layout calculations

    plots.per.page <- panel.layout[1] * panel.layout[2] 
    cols.per.page <- panel.layout[1]
    rows.per.page <- panel.layout[2]
    number.of.pages <- panel.layout[3]
    lattice.setStatus(current.plot.multipage = number.of.pages > 1)

    ## these will also eventually be 'status' variables
    current.panel.positions <- matrix(0, rows.per.page, cols.per.page)
    current.packet.positions <- matrix(0, rows.per.page, cols.per.page)
    current.cond.levels <- vector(mode = "list", length = rows.per.page * cols.per.page)
    dim(current.cond.levels) <- c(rows.per.page, cols.per.page)

    ## ## following now relegated to packet.panel 
    ## skip <- rep(x$skip, length = number.of.pages * rows.per.page * cols.per.page)

    x.alternating <- rep(x$x.scales$alternating, length = cols.per.page)
    y.alternating <- rep(x$y.scales$alternating, length = rows.per.page)
    x.relation.same <- x$x.scales$relation == "same"
    y.relation.same <- x$y.scales$relation == "same"

    ## get lists for main, sub, xlab, ylab

    main <-
        grobFromLabelList(getLabelList(x$main,
                                       trellis.par.get("par.main.text")),
                          name = trellis.grobname("main"))
    sub <-
        grobFromLabelList(getLabelList(x$sub,
                                       trellis.par.get("par.sub.text")),
                          name = trellis.grobname("sub"))
    xlab <-
        grobFromLabelList(getLabelList(x$xlab,
                                       trellis.par.get("par.xlab.text"),
                                       x$xlab.default),
                          name = trellis.grobname("xlab"))
    ylab <-
        grobFromLabelList(getLabelList(x$ylab,
                                       trellis.par.get("par.ylab.text"),
                                       x$ylab.default),
                          name = trellis.grobname("ylab"), rot = 90)


    ## get par.strip.text

    par.strip.text <- trellis.par.get("add.text")
    par.strip.text$lines <- 1
    if (!is.null(x$par.strip.text)) 
        par.strip.text[names(x$par.strip.text)] <- x$par.strip.text



    ## Shall calculate the per page Grid layout now:

    ## this layout will now be used for each page (this is quite
    ## complicated and unfortunately very convoluted)


    layoutCalculations <-
        calculateGridLayout(x,
                            rows.per.page, cols.per.page,
                            number.of.cond,
                            panel.height, panel.width,
                            main, sub,
                            xlab, ylab,
                            x.alternating, y.alternating,
                            x.relation.same, y.relation.same,
                            xaxis.rot, yaxis.rot,
                            xaxis.cex, yaxis.cex,
                            par.strip.text,
                            legend)
    page.layout <- layoutCalculations$page.layout
    pos.heights <- layoutCalculations$pos.heights
    pos.widths <- layoutCalculations$pos.widths

    n.row <- layoutNRow(page.layout)
    n.col <- layoutNCol(page.layout)


    ## commence actual plotting

    
    cond.current.level <- rep(1, number.of.cond)

    ## this vector represents the combination of levels of the
    ## conditioning variables for the current panel.  We're changing
    ## to a new scheme which should make this unnecessary, but we need
    ## to keep it for now to ensure the first page is drawn (kinda
    ## stupid, but that part of the code was there to ensure that an
    ## `empty' trellis object doesn't waste a page.  Actually, it
    ## might be redundant, since in that case number.of.pages should
    ## be 0.  FIXME: Check it out)

    
    panel.counter <- 0

    ## panel.number is available as an optional argument to the panel
    ## function (FIXME: to be deprecated).  It's a strictly increasing
    ## sequential counter keeping track of which panel is being drawn.
    ## This is usually the same as, but can be different from
    ## packet.number, which is an index to which packet combination is
    ## being used.


    ## FIXME: what happens when number of pages is 0?
    ## message("no of pages: ", number.of.pages)

    for(page.number in seq_len(number.of.pages))
    {
        ##if (!any(cond.max.levels - which.packet < 0))
        if (TRUE)
        {
            if (usual)
            {
                if (new) grid.newpage()
                grid.rect(gp = gpar(fill = bg, col = "transparent"))
                new <- TRUE
            }

            pushViewport(viewport(layout = page.layout,
                                  gp = global.gpar,
                                  name = trellis.vpname("toplevel")))


            if (!is.null(main))
            {
                drawInViewport(main,
                               viewport(layout.pos.row = pos.heights$main,
                                        name= trellis.vpname("main")))
            }
            if (!is.null(sub))
            {
                drawInViewport(sub,
                               viewport(layout.pos.row = pos.heights$sub,
                                        name= trellis.vpname("sub")))
            }
            if (!is.null(xlab))
            {
                drawInViewport(xlab,
                               viewport(layout.pos.row = pos.heights$xlab,
                                        layout.pos.col = pos.widths$panel,
                                        name= trellis.vpname("xlab")))
            }
            if (!is.null(ylab))
            {
                drawInViewport(ylab,
                               viewport(layout.pos.col = pos.widths$ylab,
                                        layout.pos.row = pos.heights$panel,
                                        name= trellis.vpname("ylab")))
            }

            last.panel <- prod(sapply(x$index.cond, length))


            ## preliminary loop through possible positions, doing some
            ## calculations that allow some helpful status variables
            ## to be set.

            ## first, initialize status variables

            current.panel.positions[, ] <- 0
            current.packet.positions[, ] <- 0
            current.cond.levels[, ] <- list(NULL)

            for (row in seq_len(rows.per.page))
                for (column in seq_len(cols.per.page))
                {
                    ## levels being used in this panel
                    which.packet <- 
                        packet.panel(layout = panel.layout,
                                     condlevels = used.condlevels,
                                     page = page.number,
                                     row = row,
                                     column = column,
                                     skip = x$skip)

                    if (!is.null(which.packet))
                    {
                        ## permute to restore original order
                        which.packet <- which.packet[inverse.permutation]

                        current.cond.levels[[row, column]] <- which.packet

                        ## packet.number should be same as packet.array[which.packet]
                        ##                                              ^^^^^^^^^^^
                        ##                                          (length not fixed)

                        packet.number <- 
                            do.call("[", c(list(x = packet.array), as.list(which.packet)))
                        current.packet.positions[row, column] <- packet.number

                        ## packet.number retrieves the appropriate
                        ## entry of panel.args and [xy].limits. It has
                        ## to be this way because otherwise
                        ## non-trivial orderings will not work.

                        ## But we also provide a simple incremental
                        ## counter that may be used as a panel
                        ## function argument

                        panel.counter <- panel.counter + 1
                        current.panel.positions[row, column] <- panel.counter
                    }
                }

            lattice.setStatus(current.cond.levels = current.cond.levels)
            lattice.setStatus(current.panel.positions = current.panel.positions)
            lattice.setStatus(current.packet.positions = current.packet.positions)

            ## loop through positions again, doing the actual drawing
            ## this time

            for (row in seq_len(rows.per.page))
                for (column in seq_len(cols.per.page))
                {
                    lattice.setStatus(current.focus.row = row,
                                      current.focus.column = column)
                    which.packet <- which.packet()
                    if (!is.null(which.packet))
                    {
                        packet.number <- packet.number()
                        panel.number <- panel.number() ## needed? FIXME

                        ## this gives the row position from the bottom
                        actual.row <- if (x$as.table)
                            (rows.per.page-row+1) else row

                        pos.row <- pos.heights$panel[row]
                        pos.col <- pos.widths$panel[column]

                        xscale.comps <-
                            if (x.relation.same)
                                x$xscale.components(lim = x$x.limits, 
                                                    ## (FIXME: needs work) packet.list = ...
                                                    top = TRUE,

                                                    ## rest passed on to
                                                    ## calculateAxisComponents
                                                    ## in the default
                                                    ## case:
                                                    at = x$x.scales$at,
                                                    used.at = x$x.used.at,
                                                    num.limit = x$x.num.limit,
                                                    labels = x$x.scales$lab,
                                                    logsc = x$x.scales$log,
                                                    abbreviate = x$x.scales$abbr,
                                                    minlength = x$x.scales$minl,
                                                    n = x$x.scales$tick.number,
                                                    format.posixt = x$x.scales$format)
                            else 
                                x$xscale.components(lim = x$x.limits[[packet.number]], 
                                                    ## FIXME: needs work packet.list = ...
                                                    top = FALSE,

                                                    ## rest passed on to
                                                    ## calculateAxisComponents
                                                    ## in the default
                                                    ## case:

                                                    at = if (is.list(x$x.scales$at))
                                                    x$x.scales$at[[packet.number]]
                                                    else x$x.scales$at,
                                                    used.at = x$x.used.at[[packet.number]],
                                                    num.limit = x$x.num.limit[[packet.number]],
                                                    labels =
                                                    if (is.list(x$x.scales$lab))
                                                    x$x.scales$lab[[packet.number]]
                                                    else x$x.scales$lab,
                                                    logsc = x$x.scales$log,
                                                    abbreviate = x$x.scales$abbr,
                                                    minlength = x$x.scales$minl,
                                                    n = x$x.scales$tick.number,
                                                    format.posixt = x$x.scales$format)


                        yscale.comps <-
                            if (y.relation.same)
                                x$yscale.components(lim = x$y.limits, 
                                                    ## FIXME: needs work packet.list = ...
                                                    right = TRUE,

                                                    ## rest passed on to
                                                    ## calculateAxisComponents
                                                    ## in the default
                                                    ## case:
                                                    at = x$y.scales$at,
                                                    used.at = x$y.used.at,
                                                    num.limit = x$y.num.limit,
                                                    labels = x$y.scales$lab,
                                                    logsc = x$y.scales$log,
                                                    abbreviate = x$y.scales$abbr,
                                                    minlength = x$y.scales$minl,
                                                    n = x$y.scales$tick.number,
                                                    format.posixt = x$y.scales$format)
                            else 
                                x$yscale.components(lim = x$y.limits[[packet.number]], 
                                                    ## FIXME: needs work packet.list = ...
                                                    right = FALSE,

                                                    ## rest passed on to
                                                    ## calculateAxisComponents
                                                    ## in the default
                                                    ## case:

                                                    at = if (is.list(x$y.scales$at))
                                                    x$y.scales$at[[packet.number]]
                                                    else x$y.scales$at,
                                                    used.at = x$y.used.at[[packet.number]],
                                                    num.limit = x$y.num.limit[[packet.number]],
                                                    labels =
                                                    if (is.list(x$y.scales$lab))
                                                    x$y.scales$lab[[packet.number]]
                                                    else x$y.scales$lab,
                                                    logsc = x$y.scales$log,
                                                    abbreviate = x$y.scales$abbr,
                                                    minlength = x$y.scales$minl,
                                                    n = x$y.scales$tick.number,
                                                    format.posixt = x$y.scales$format)


                        xscale <- xscale.comps$num.limit
                        yscale <- yscale.comps$num.limit

############################################
###        drawing the axes               ##
############################################


### whether or not axes are drawn, we'll create viewports for them
### anyway, so that users can later interactively add axes/other stuff
### if they wish.  First up, we'll have a 'strip.column.row.off'
### viewport for the top axes, then another one for those on the left
### (there can be strips on the left too), and then a
### 'panel.column.row.off' viewport for the other 2 (no strips allowed
### there).  The names may seem a bit unintuitive, and perhaps they
### are, but some justification is provided in help(trellis.focus)


                        pushViewport(viewport(layout.pos.row = pos.row - 1,
                                              layout.pos.col = pos.col,
                                              xscale = xscale,
                                              clip = "off",
                                              name =
                                              trellis.vpname("strip",
                                                             column = column,
                                                             row = row,
                                                             clip.off = TRUE)))
                        ## X-axis above
                        x$axis(side = "top",
                               scales = x$x.scales,
                               components = xscale.comps,
                               as.table = x$as.table,
                               rot = xaxis.rot[2],
                               text.col = xaxis.col.text,
                               text.alpha = xaxis.alpha.text,
                               text.cex = xaxis.cex[2],
                               text.font = xaxis.font,
                               text.fontfamily = xaxis.fontfamily,
                               text.fontface = xaxis.fontface,
                               line.col = xaxis.col.line,
                               line.lty = xaxis.lty,
                               line.lwd = xaxis.lwd,
                               line.alpha = xaxis.alpha.line)
                        upViewport()

                        ## Y-axis to the left
                        pushViewport(viewport(layout.pos.row = pos.row,
                                              layout.pos.col = pos.col - 1,
                                              yscale = yscale,
                                              clip = "off",
                                              name =
                                              trellis.vpname("strip.left",
                                                             column = column,
                                                             row = row,
                                                             clip.off = TRUE)))
                        x$axis(side = "left",
                               scales = x$y.scales,
                               components = yscale.comps,
                               as.table = x$as.table,
                               rot = yaxis.rot[1],
                               text.col = yaxis.col.text,
                               text.alpha = yaxis.alpha.text,
                               text.cex = yaxis.cex[1],
                               text.font = yaxis.font,
                               text.fontfamily = yaxis.fontfamily,
                               text.fontface = yaxis.fontface,
                               line.col = yaxis.col.line,
                               line.lty = yaxis.lty,
                               line.lwd = yaxis.lwd,
                               line.alpha = yaxis.alpha.line)
                        upViewport()



                        ## X-axis bottom and Y-axis right
                        pushViewport(viewport(layout.pos.row = pos.row,
                                              layout.pos.col = pos.col,
                                              xscale = xscale,
                                              yscale = yscale,
                                              clip = "off",
                                              name =
                                              trellis.vpname("panel",
                                                             column = column,
                                                             row = row,
                                                             clip.off = TRUE)))
                        ## X-axis below
                        x$axis(side = "bottom",
                               scales = x$x.scales,
                               components = xscale.comps,
                               as.table = x$as.table,
                               rot = xaxis.rot[1],
                               text.col = xaxis.col.text,
                               text.alpha = xaxis.alpha.text,
                               text.cex = xaxis.cex[1],
                               text.font = xaxis.font,
                               text.fontfamily = xaxis.fontfamily,
                               text.fontface = xaxis.fontface,
                               line.col = xaxis.col.line,
                               line.lty = xaxis.lty,
                               line.lwd = xaxis.lwd,
                               line.alpha = xaxis.alpha.line)
                        ## Y-axis to the right
                        x$axis(side = "right",
                               scales = x$y.scales,
                               components = yscale.comps,
                               as.table = x$as.table,
                               rot = yaxis.rot[2],
                               text.col = yaxis.col.text,
                               text.alpha = yaxis.alpha.text,
                               text.cex = yaxis.cex[2],
                               text.font = yaxis.font,
                               text.fontfamily = yaxis.fontfamily,
                               text.fontface = yaxis.fontface,
                               line.col = yaxis.col.line,
                               line.lty = yaxis.lty,
                               line.lwd = yaxis.lwd,
                               line.alpha = yaxis.alpha.line)

### While we're at it, we'll also draw the box around panels.  This
### used to be done with clipping on, which had lots of subtle and
### puzzling side effects.


                        grid.rect(gp =
                                  gpar(col = axis.line$col,
                                       lty = axis.line$lty,
                                       lwd = axis.line$lwd,
                                       alpha = axis.line$alpha,
                                       fill = "transparent"))



                        upViewport()


                        
############################################
###        done drawing axes              ##
############################################



############################################
###        drawing the panel              ##
############################################


                        pushViewport(viewport(layout.pos.row = pos.row,
                                              layout.pos.col = pos.col,
                                              xscale = xscale,
                                              yscale = yscale,
                                              clip = trellis.par.get("clip")$panel,
                                              name =
                                              trellis.vpname("panel",
                                                             column = column,
                                                             row = row,
                                                             clip.off = FALSE)))


                        pargs <- c(x$panel.args[[packet.number]],
                                   x$panel.args.common) #,
##                                    list(packet.number = packet.number,
##                                         panel.number = panel.number))

##########################################
### FIXME: remove this at some point   ###
###                                    ###
                        if (any(c("packet.number", "panel.number") %in% names(formals(panel))))
                        {
                            warning("'packet.number' and 'panel.number' are no longer supplied to the panel function.  See ?packet.number")
                        }
###                                    ###
##########################################


                        if (!("..." %in% names(formals(panel))))
                            pargs <- pargs[names(formals(panel))]
                        do.call("panel", pargs)

                        upViewport()


############################################
###       finished drawing panel          ##
############################################




#########################################
###      draw strip(s) on top         ###
#########################################

                        if (!is.logical(strip)) # logical <==> FALSE
                        {
                            pushViewport(viewport(layout.pos.row = pos.row - 1,
                                                  layout.pos.col = pos.col,
                                                  clip = trellis.par.get("clip")$strip,
                                                  name =
                                                  trellis.vpname("strip",
                                                                 column = column,
                                                                 row = row,
                                                                 clip.off = FALSE)))
                            for(i in seq_len(number.of.cond))
                            {
                                ## Here, by which.given, I mean which
                                ## in the original order, not the
                                ## permuted order

                                strip(which.given = x$perm.cond[i],
                                      which.panel = which.packet,
##                                       panel.number = panel.number,
##                                       packet.number = packet.number,

                                      var.name = names(x$condlevels),
                                      factor.levels = as.character(x$condlevels[[x$perm.cond[i]]]),

                                      shingle.intervals = if (is.list(x$condlevels[[x$perm.cond[i]]]))
                                      do.call("rbind", x$condlevels[[x$perm.cond[i]]]) else NULL,

                                      bg = strip.col.default.bg[i],
                                      fg = strip.col.default.fg[i],
                                      par.strip.text = par.strip.text)
                                
                            }
                            upViewport()
                        }




#########################################
###        draw strip(s) on left      ###
#########################################


                        if (!is.logical(strip.left)) # logical <==> FALSE
                        {
                            pushViewport(viewport(layout.pos.row = pos.row,
                                                  layout.pos.col = pos.col - 1,
                                                  clip = trellis.par.get("clip")$strip,
                                                  name =
                                                  trellis.vpname("strip.left",
                                                                 column = column,
                                                                 row = row,
                                                                 clip.off = FALSE)))


                            for(i in seq_len(number.of.cond))
                            {
                                ## Here, by which.given, I mean which
                                ## in the original packet order, not
                                ## the permuted order

                                strip.left(which.given = x$perm.cond[i],
                                           which.panel = which.packet,
##                                            panel.number = panel.number,
##                                            packet.number = packet.number,

                                           var.name = names(x$condlevels),
                                           factor.levels = as.character(x$condlevels[[x$perm.cond[i]]]),

                                           shingle.intervals = if (is.list(x$condlevels[[x$perm.cond[i]]]))
                                           do.call("rbind", x$condlevels[[x$perm.cond[i]]]) else NULL,

                                           bg = strip.col.default.bg[i],
                                           fg = strip.col.default.fg[i],
                                           par.strip.text = par.strip.text)
                            }
                            upViewport()
                        }
                    }
                }


            ## legend / key plotting

            if (!is.null(legend))
            {
                locs <- names(legend)
                for (i in seq_along(legend))
                {
                    key.space <- locs[i]
                    key.gf <- legend[[i]]$obj

                    switch(key.space,
                           left = 
                           drawInViewport(key.gf,
                                          viewport(layout.pos.col = pos.widths$key.left,
                                                   layout.pos.row = range(pos.heights$panel, pos.heights$strip),
                                                   name = trellis.vpname("legend", side = "left"))),
                           right = 
                           drawInViewport(key.gf,
                                          viewport(layout.pos.col = pos.widths$key.right,
                                                   layout.pos.row = range(pos.heights$panel, pos.heights$strip),
                                                   name = trellis.vpname("legend", side = "right"))),
                           top = 
                           drawInViewport(key.gf,
                                          viewport(layout.pos.row = pos.heights$key.top,
                                                   layout.pos.col = range(pos.widths$panel, pos.widths$strip.left),
                                                   name = trellis.vpname("legend", side = "top"))),
                           bottom = 
                           drawInViewport(key.gf,
                                          viewport(layout.pos.row = pos.heights$key.bottom,
                                                   layout.pos.col = range(pos.widths$panel, pos.widths$strip.left),
                                                   name = trellis.vpname("legend", side = "bottom"))),
                           inside = {

                               ## FIXME: there are two choices here
                               ## --- either treat the whole figure
                               ## region as the rectangle, or use just
                               ## the region covered by the panels.
                               ## Currently, the first is done.
                               ## Ideally, it should be
                               ## user-controllable, presumably
                               ## through lattice.options().  Easy to
                               ## do, just need to figure out the
                               ## limits.  Just not feeling
                               ## enthusiastic enough right now.

                               pushViewport(viewport(layout.pos.row = c(1, n.row),
                                                     layout.pos.col = c(1, n.col),
                                                     name = trellis.vpname("legend", side = "inside")))
                               key.corner <-
                                   if (is.null(legend[[i]]$corner)) c(0,1)
                                   else legend[[i]]$corner
                               key.x <- 
                                   if (is.null(legend[[i]]$x)) key.corner[1]
                                   else legend[[i]]$x
                               key.y <- 
                                   if (is.null(legend[[i]]$y)) key.corner[2]
                                   else legend[[i]]$y
                               if (all(key.corner == c(0,1)))
                               {
                                   pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                         widths = unit(c(key.x, 1, 1),
                                                         c("npc", "grobwidth", "null"),
                                                         list(NULL, key.gf, NULL)),
                                                         heights = unit(c(1 - key.y, 1, 1),
                                                         c("npc", "grobheight", "null"),
                                                         list(NULL, key.gf, NULL)))))
                               }
                               else if (all(key.corner == c(1,1)))
                               {
                                   pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                         heights = unit(c(1 - key.y, 1, 1),
                                                         c("npc", "grobheight", "null"),
                                                         list(NULL, key.gf, NULL)),
                                                         widths = unit(c(1, 1, 1 - key.x),
                                                         c("null", "grobwidth", "npc"),
                                                         list(NULL, key.gf, NULL)))))
                               }
                               else if (all(key.corner == c(0,0)))
                               {
                                   pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                         widths = unit(c(key.x, 1, 1),
                                                         c("npc", "grobwidth", "null"),
                                                         list(NULL, key.gf, NULL)),
                                                         heights = unit(c(1, 1, key.y),
                                                         c("null", "grobheight", "npc"),
                                                         list(NULL, key.gf, NULL)))))
                               }
                               else if (all(key.corner == c(1,0)))
                               {
                                   pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 3,
                                                         widths = unit(c(1, 1, 1 - key.x),
                                                         c("null", "grobwidth", "npc"),
                                                         list(NULL, key.gf, NULL)),
                                                         heights = unit(c(1, 1, key.y),
                                                         c("null", "grobheight", "npc"),
                                                         list(NULL, key.gf, NULL)))))
                               }
                               drawInViewport(key.gf,
                                              viewport(layout.pos.row = 2, layout.pos.col = 2))
                               upViewport(2)
                           })
                       }
                }
                pushViewport(viewport(layout.pos.row = c(1, n.row),
                                      layout.pos.col = c(1, n.col),
                                      name = trellis.vpname("page")))
                if (!is.null(x$page)) x$page(page.number)                
                upViewport()
                upViewport()
            }
        }
        if (!missing(position))
        {
            if (!missing(split))
            {
                upViewport()
                upViewport()
            }
            upViewport()
        }
        else if (!missing(split))
        {
            upViewport()
            upViewport()
        }

    ## restore earlier settings
    if (!is.null(x$lattice.options))
    {
        lattice.options(oopt)
    }
    if (!is.null(x$par.settings))
    {
        trellis.par.set(opar, strict = TRUE)
    }

    if (!is.null(draw.in)) upViewport(depth)

    lattice.setStatus(current.focus.row = 0,
                      current.focus.column = 0,
                      vp.highlighted = FALSE,
                      vp.depth = 0)
    invisible(x)
}



