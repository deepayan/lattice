


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



## accessors for a grid layouts nrow and ncol

layoutNRow <- function(x) x$nrow
layoutNCol <- function(x) x$ncol





## utility to create a full-fledged list describing a label from parts
## (used for main, sub, xlab, ylab)

getLabelList <- function(label, text.settings, default.label = NULL)
{
    if (!is.null(label))
    {
        if (inherits(label, "grob")) return(label)
        ans <- list(label = 
                    if (is.characterOrExpression(label)) label
                    else if (is.list(label) && (is.null(names(label)) || names(label)[1] == "")) label[[1]]
                    else default.label,
                    col = text.settings$col, cex = text.settings$cex,
                    fontfamily = text.settings$fontfamily,
                    fontface = text.settings$fontface,
                    font = text.settings$font)
        if (is.list(label)) ans[names(label)] <- label
    }
    else ans <- NULL
    if (is.null(ans$lab) ||
        (is.character(ans) && ans$lab == "")) ans <- NULL
    ans
}






grobFromLabelList <- function(lab, name = "label", rot = 0)
{
    if (is.null(lab)) return (NULL)
    if (inherits(lab, "grob")) return(lab)

    textGrob(label = lab$label, name= name, rot = rot,
             gp =
             gpar(col = lab$col,
                  fontfamily = lab$fontfamily,
                  fontface = chooseFace(lab$fontface, lab$font),
                  cex = lab$cex))
}






evaluate.legend <- function(legend)
{
    if (is.null(legend)) return(NULL)
    for (i in seq(along = legend))
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














## S3 print method for "trellis" objects

print.trellis <-
    function(x, position, split, more = FALSE,
             newpage = TRUE,
             panel.height = lattice.getOption("layout.heights")$panel,
             panel.width = lattice.getOption("layout.widths")$panel,
             save.object = lattice.getOption("save.object"),
             prefix,
             ...)
{
    if (is.null(dev.list())) trellis.device()
    else if (is.null(trellis.par.get()))
        trellis.device(device = .Device, new = FALSE)

    ## if necessary, save current settings and apply temporary
    ## settings in x$par.settings

    if (!is.null(x$par.settings))
    {
        opars <- trellis.par.get()
        trellis.par.set(theme = x$par.settings)
    }

    bg <- trellis.par.get("background")$col
    new <-  newpage && !lattice.getStatus("print.more")
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

    fontsize.text <- trellis.par.get("fontsize")$text

    if (!missing(position))
    {
        if (length(position)!=4) stop("Incorrect value of position")
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
            if (length(split)!=4) stop("Incorrect value of split")
            pushViewport(viewport(layout = grid.layout(nrow = split[4], ncol = split[3]),
                                  name = trellis.vpname("split") ))
            pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1],
                                  name = trellis.vpname("split.location") ))
        }
    }


    else if (!missing(split))
    {
        if (length(split)!=4) stop("Incorrect value of split")
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

    order.cond <- seq(length = prod(sapply(x$condlevels, length)))
    dim(order.cond) <- sapply(x$condlevels, length)

    ## first subset, then permute
    order.cond <- do.call("[", c(list(order.cond), x$index.cond, list(drop = FALSE)))
    order.cond <- aperm(order.cond, perm = x$perm.cond)

    ## order.cond will be used as indices for (exactly) the following

    ## 1. panel.args
    ## 2. x.limits
    ## 3. y.limits

    ## may need x$(subset|perm).cond later for strip drawing

    cond.max.level <- dim(order.cond)
    number.of.cond <- length(cond.max.level)

    panel.layout <-
        compute.layout(x$layout, cond.max.level, skip = x$skip)

    panel <- # shall use "panel" in do.call
        if (is.function(x$panel)) x$panel 
        else if (is.character(x$panel)) get(x$panel)
        else eval(x$panel)

    strip <- 
        if (is.function(x$strip)) x$strip 
        else if (is.character(x$strip)) get(x$strip)
        else eval(x$strip)

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
        rep(trellis.par.get("strip.background")$col, length=number.of.cond)
    strip.col.default.fg <-
        rep(trellis.par.get("strip.shingle")$col,length=number.of.cond)
    strip.border <-
        lapply(trellis.par.get("strip.border"),
               function(x) rep(x, length=number.of.cond))


    ## Start layout calculations when only number of panels per page
    ## is pecified (this refers to the layout argument, not grid
    ## layouts)

    if (panel.layout[1] == 0) # using device dimensions to
    {
        ddim <- par("din") # calculate default layout
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
    ## WAS: else, but then things may become inconsistent with skip
    plots.per.page <- panel.layout[1] * panel.layout[2] 

    ## End layout calculations


    cols.per.page <- panel.layout[1]
    rows.per.page <- panel.layout[2]
    number.of.pages <- panel.layout[3]
    lattice.setStatus(current.plot.multipage = number.of.pages > 1)
    ## this will also eventually be a 'status' variable
    current.panel.positions <- matrix(0, rows.per.page, cols.per.page)


    skip <- rep(x$skip, length = number.of.pages * rows.per.page * cols.per.page)

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

    ##   this vector represents the combination of levels of the
    ##   conditioning variables for the current panel.

    
    panel.counter <- 0

    ## panel.counter used as an optional argument to the panel
    ## function. Sequential counter keeping track of which panel is
    ## being drawn

    for(page.number in 1:number.of.pages)
    {
        if (!any(cond.max.level - cond.current.level < 0)) {
            
            if (usual)
            {
                if (new) grid.newpage()
                grid.rect(gp = gpar(fill = bg, col = "transparent"))
                new <- TRUE
            }

            pushViewport(viewport(layout = page.layout,
                                  gp =
                                  gpar(fontsize = fontsize.text),
                                  name = trellis.vpname("toplevel")))



#             if (!is.null(main))
#                 grid.text(label = main$label, name= "main",
#                           gp =
#                           gpar(col = main$col,
#                                fontfamily = main$fontfamily,
#                                fontface = chooseFace(main$fontface, main$font),
#                                cex = main$cex),
#                           vp = viewport(layout.pos.row = pos.heights$main, name= "main.vp"))

#             if (!is.null(sub))
#                 grid.text(label = sub$label, name= "sub",
#                           gp =
#                           gpar(col = sub$col,
#                                fontfamily = sub$fontfamily,
#                                fontface = chooseFace(sub$fontface, sub$font),
#                                cex = sub$cex),
#                           vp = viewport(layout.pos.row = pos.heights$sub, name= "sub.vp"))

#             if (!is.null(xlab))
#                 grid.text(label = xlab$label, name= "xlab",
#                           gp =
#                           gpar(col = xlab$col,
#                                fontfamily = xlab$fontfamily,
#                                fontface = chooseFace(xlab$fontface, xlab$font),
#                                cex = xlab$cex), 
#                           vp = viewport(layout.pos.row = pos.heights$xlab,
#                                         layout.pos.col = pos.widths$panel, name= "xlab.vp" ))

#             if (!is.null(ylab))
#                 grid.text(label = ylab$label, rot = 90, name= "ylab",
#                           gp =
#                           gpar(col = ylab$col,
#                                fontfamily = ylab$fontfamily,
#                                fontface = chooseFace(ylab$fontface, ylab$font),
#                                cex = ylab$cex),
#                           vp = viewport(layout.pos.col = pos.widths$ylab,
#                                         layout.pos.row = pos.heights$panel, name= "ylab.vp"))




            if (!is.null(main))
            {
                pushViewport(viewport(layout.pos.row = pos.heights$main,
                                      name= trellis.vpname("main")))
                grid.draw(main)
                upViewport()
            }
            if (!is.null(sub))
            {
                pushViewport(viewport(layout.pos.row = pos.heights$sub,
                                      name= trellis.vpname("sub")))
                grid.draw(sub)
                upViewport()
            }
            if (!is.null(xlab))
            {
                pushViewport(viewport(layout.pos.row = pos.heights$xlab,
                                      layout.pos.col = pos.widths$panel,
                                      name= trellis.vpname("xlab")))
                grid.draw(xlab)
                upViewport()
            }
            if (!is.null(ylab))
            {
                pushViewport(viewport(layout.pos.col = pos.widths$ylab,
                                      layout.pos.row = pos.heights$panel,
                                      name= trellis.vpname("ylab")))
                grid.draw(ylab)
                upViewport()
            }


            last.panel <- prod(sapply(x$index.cond, length))

            for (row in seq(length = rows.per.page))
                for (column in seq(length = cols.per.page))
                {
                    if (!any(cond.max.level-cond.current.level<0) &&
                        (row-1) * cols.per.page + column <= plots.per.page &&
                        !skip[(page.number-1) * rows.per.page * cols.per.page +
                              (row-1) * cols.per.page + column] )
                    {
                        ##panel.number should be same as order.cond[cond.current.level]
                        ##                                          ^^^^^^^^^^^^^^^^^^
                        ##                                          (length not fixed)

                        panel.number <- 
                            do.call("[", c(list(x = order.cond), as.list(cond.current.level)))
                        current.panel.positions[row, column] <- panel.number

                        ## this index retrieves the appropriate entry
                        ## of panel.args and [xy].limits. It has to be
                        ## this way because otherwise non-trivial
                        ## orderings will not work.

                        ## But we should also have a simple
                        ## incremental counter that may be used as a
                        ## panel function argument

                        panel.counter <- panel.counter + 1

                        ## this gives the row position from the bottom
                        actual.row <- if (x$as.table)
                            (rows.per.page-row+1) else row

                        pos.row <- pos.heights$panel[row]
                        pos.col <- pos.widths$panel[column]

                        xlabelinfo <-
                            calculateAxisComponents(x =
                                                    if (x.relation.same) x$x.limits
                                                    else x$x.limits[[panel.number]],

                                                    at = if (is.list(x$x.scales$at))
                                                    x$x.scales$at[[panel.number]]
                                                    else x$x.scales$at,

                                                    used.at = if (!x.relation.same)
                                                    x$x.used.at[[panel.number]] else x$x.used.at,

                                                    num.limit = if (!x.relation.same)
                                                    x$x.num.limit[[panel.number]] else x$x.num.limit,

                                                    labels =
                                                    if (is.list(x$x.scales$lab))
                                                    x$x.scales$lab[[panel.number]]
                                                    else x$x.scales$lab,

                                                    logsc = x$x.scales$log,
                                                    abbreviate = x$x.scales$abbr,
                                                    minlength = x$x.scales$minl,
                                                    n = x$x.scales$tick.number,
                                                    format.posixt = x$x.scales$format)

                        ylabelinfo <-
                            calculateAxisComponents(x =
                                                    if (y.relation.same) x$y.limits
                                                    else x$y.limits[[panel.number]],

                                                    at = if (is.list(x$y.scales$at))
                                                    x$y.scales$at[[panel.number]]
                                                    else x$y.scales$at,

                                                    used.at = if (!y.relation.same)
                                                    x$y.used.at[[panel.number]] else x$y.used.at,

                                                    num.limit = if (!y.relation.same)
                                                    x$y.num.limit[[panel.number]] else x$y.num.limit,

                                                    labels =
                                                    if (is.list(x$y.scales$lab))
                                                    x$y.scales$lab[[panel.number]]
                                                    else x$y.scales$lab,

                                                    logsc = x$y.scales$log,
                                                    abbreviate = x$y.scales$abbr,
                                                    minlength = x$y.scales$minl,
                                                    n = x$y.scales$tick.number,
                                                    format.posixt = x$y.scales$format)

                        xscale <- xlabelinfo$num.limit
                        yscale <- ylabelinfo$num.limit


############################################
###        drawing the axes               ##
############################################


### whether or not axes are drawn, we'll create viewports for them
### anyway, so that users can later interactively add axes/other stuff
### if they wish.  First up, we'll have a 'strip.column.row.off'
### viewport for the top axes, and then a 'panel.column.row.off'
### viewport for the other 3.  The names may seem a bit unintuitive,
### and perhaps they are, but some justification is provided in
### help(trellis.focus)


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
                        if (x$x.scales$draw && x.relation.same && actual.row == rows.per.page)
                        {
                            axstck <- x$x.scales$tck
                            panel.axis(side = "top",
                                       at = xlabelinfo$at,
                                       labels = xlabelinfo$lab,
                                       draw.labels = (x.alternating[column]==2 ||
                                                      x.alternating[column]==3), 
                                       check.overlap = xlabelinfo$check.overlap,
                                       outside = TRUE,
                                       tick = TRUE,
                                       tck = axstck[2],
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
                        }
                        upViewport()


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
                        if (x$x.scales$draw && (!x.relation.same || actual.row == 1))
                        {
                            axstck <- x$x.scales$tck
                            panel.axis(side = "bottom",
                                       at = xlabelinfo$at,
                                       labels = xlabelinfo$lab,
                                       draw.labels = (!x.relation.same ||
                                                      x.alternating[column]==1 ||
                                                      x.alternating[column]==3), 
                                       check.overlap = xlabelinfo$check.overlap,
                                       outside = TRUE,
                                       tick = TRUE,
                                       tck = axstck[1],
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
                        }




                        ## Y-axis
                        
                        ## Y-axis to the left
                        if (x$y.scales$draw && (!y.relation.same || column == 1))
                        {
                            axstck <- x$y.scales$tck
                            panel.axis(side = "left",
                                       at = ylabelinfo$at,
                                       labels = ylabelinfo$lab,
                                       draw.labels = (!y.relation.same ||
                                                      y.alternating[actual.row]==1 ||
                                                      y.alternating[actual.row]==3), 
                                       check.overlap = ylabelinfo$check.overlap,
                                       outside = TRUE,
                                       tick = TRUE,
                                       tck = axstck[1],
                                       rot = yaxis.rot[1],
                                       text.col = yaxis.col.text,
                                       text.alpha = yaxis.alpha.text,
                                       text.cex = yaxis.cex[1],
                                       text.font = yaxis.font,
                                       text.fontfamily = yaxis.fontfamily,
                                       text.fontface = xaxis.fontface,
                                       line.col = yaxis.col.line,
                                       line.lty = yaxis.lty,
                                       line.lwd = yaxis.lwd,
                                       line.alpha = yaxis.alpha.line)
                        }

                        ## Y-axis to the right

                        ## A special case where we need to do this is
                        ## when the panel is the last one on the page.
                        ## Unfortunately, I can't think of an easy way
                        ## to determine this.  One thing I could do,
                        ## and one that should cover most reasonable
                        ## cases, is to do this for the absolutely
                        ## last panel.

                        if (x$y.scales$draw && y.relation.same &&
                            (column == cols.per.page || panel.counter == last.panel))
                        {
                            axstck <- x$y.scales$tck
                            panel.axis(side = "right",
                                       at = ylabelinfo$at,
                                       labels = ylabelinfo$lab,
                                       draw.labels = (y.alternating[actual.row]==2 ||
                                                      y.alternating[actual.row]==3), 
                                       check.overlap = ylabelinfo$check.overlap,
                                       outside = TRUE,
                                       tick = TRUE,
                                       tck = axstck[2],
                                       rot = yaxis.rot[2],
                                       text.col = yaxis.col.text,
                                       text.alpha = yaxis.alpha.text,
                                       text.cex = yaxis.cex[2],
                                       text.font = yaxis.font,
                                       text.fontfamily = yaxis.fontfamily,
                                       text.fontface = xaxis.fontface,
                                       line.col = yaxis.col.line,
                                       line.lty = yaxis.lty,
                                       line.lwd = yaxis.lwd,
                                       line.alpha = yaxis.alpha.line)
                        }
                        upViewport()



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


                        pargs <- c(x$panel.args[[panel.number]],
                                   x$panel.args.common,
                                   list(panel.number = panel.number,
                                        panel.counter = panel.counter))

                        if (!("..." %in% names(formals(panel))))
                            pargs <- pargs[names(formals(panel))]
                        do.call("panel", pargs)


                                
                        grid.rect(gp =
                                  gpar(col = axis.line$col,
                                       lty = axis.line$lty,
                                       lwd = axis.line$lwd,
                                       alpha = axis.line$alpha,
                                       fill = "transparent"))

                        upViewport()


############################################
###       finished drawing panel          ##
############################################








#########################################
###        draw strip(s)              ###
#########################################

                        if (!is.logical(strip)) # logical <==> FALSE
                        {
                            ## which.panel in original cond variables order
                            which.panel = cond.current.level[x$perm.cond]

                            ## need to pass each index in original terms
                            for (i in seq(along = which.panel))
                                which.panel[i] <- x$index.cond[[i]][which.panel[i]]

                            pushViewport(viewport(layout.pos.row = pos.row - 1,
                                                  layout.pos.col = pos.col,
                                                  clip = trellis.par.get("clip")$strip,
                                                  name =
                                                  trellis.vpname("strip",
                                                                 column = column,
                                                                 row = row,
                                                                 clip.off = FALSE)))


                            for(i in seq(length = number.of.cond))
                            {

                                ## Here, by which.given, I mean which
                                ## in the original order, not the
                                ## permuted order

                                strip(which.given = x$perm.cond[i],
                                      which.panel = which.panel,

                                      var.name = names(x$condlevels),

                                      factor.levels = if (!is.list(x$condlevels[[x$perm.cond[i]]]))
                                      x$condlevels[[x$perm.cond[i]]] else NULL,

                                      shingle.intervals = if (is.list(x$condlevels[[x$perm.cond[i]]]))
                                      do.call("rbind", x$condlevels[[x$perm.cond[i]]]) else NULL,

                                      bg = strip.col.default.bg[i],
                                      fg = strip.col.default.fg[i],
                                      par.strip.text = par.strip.text)
                                
                            }
                            upViewport()
                        }

                        
                        cond.current.level <- cupdate(cond.current.level,
                                                      cond.max.level)

                    }
                }






            ## legend / key plotting

            if (!is.null(legend))
            {
                locs <- names(legend)
                for (i in seq(along = legend))
                {
                    key.space <- locs[i]
                    key.gf <- legend[[i]]$obj

                    if (key.space == "left")
                    {
                        pushViewport(viewport(layout.pos.col = pos.widths$key.left,
                                              layout.pos.row = range(pos.heights$panel, pos.heights$strip),
                                              name = trellis.vpname("legend", side = "left")))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "right")
                    {
                        pushViewport(viewport(layout.pos.col = pos.widths$key.right,
                                              layout.pos.row = range(pos.heights$panel, pos.heights$strip),
                                              name = trellis.vpname("legend", side = "right")))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "top")
                    {
                        pushViewport(viewport(layout.pos.row = pos.heights$key.top,
                                              layout.pos.col = pos.widths$panel,
                                              name = trellis.vpname("legend", side = "top")))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "bottom")
                    {
                        pushViewport(viewport(layout.pos.row = pos.heights$key.bottom,
                                              layout.pos.col = pos.widths$panel,
                                              name = trellis.vpname("legend", side = "bottom")))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "inside")
                    {
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
                        
                        if (all(key.corner == c(0,1))) {
                            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                  widths = unit(c(key.x, 1, 1),
                                                  c("npc", "grobwidth", "null"),
                                                  list(1, key.gf, 1)),
                                                  heights = unit(c(1 - key.y, 1, 1),
                                                  c("npc", "grobheight", "null"),
                                                  list(1, key.gf, 1)))))
                        }
                        else if (all(key.corner == c(1,1))) {
                            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                  heights = unit(c(1 - key.y, 1, 1),
                                                  c("npc", "grobheight", "null"),
                                                  list(1, key.gf, 1)),
                                                  widths = unit(c(1, 1, 1 - key.x),
                                                  c("null", "grobwidth", "npc"),
                                                  list(1, key.gf, 1)))))
                        }
                        else if (all(key.corner == c(0,0))) {
                            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                  widths = unit(c(key.x, 1, 1),
                                                  c("npc", "grobwidth", "null"),
                                                  list(1, key.gf, 1)),
                                                  heights = unit(c(1, 1, key.y),
                                                  c("null", "grobheight", "npc"),
                                                  list(1, key.gf, 1)))))
                        }
                        else if (all(key.corner == c(1,0))) {
                            pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 3,
                                                  widths = unit(c(1, 1, 1 - key.x),
                                                  c("null", "grobwidth", "npc"),
                                                  list(1, key.gf, 1)),
                                                  heights = unit(c(1, 1, key.y),
                                                  c("null", "grobheight", "npc"),
                                                  list(1, key.gf, 1)))))
                        }
                        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
                        grid.draw(key.gf)
                        upViewport(3)
                    }
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
    if (!missing(position)) {
        if (!missing(split)) {
            upViewport()
            upViewport()
        }
        upViewport()
    }
    else if (!missing(split)) {
        upViewport()
        upViewport()
    }

    if (!is.null(x$par.settings))
    {
        trellis.par.set(theme = opars)
    }

    if (save.object)
    {
        assign("last.object", x, env = .LatticeEnv)
        lattice.setStatus(current.plot.saved = TRUE)
    }
    else
        lattice.setStatus(current.plot.saved = FALSE)
    lattice.setStatus(current.panel.positions = current.panel.positions)
    lattice.setStatus(current.focus.row = 0,
                      current.focus.column = 0,
                      vp.highlighted = FALSE,
                      vp.depth = 0)
    invisible(x)
}



