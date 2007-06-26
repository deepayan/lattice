

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
### Copyright (C) 2007 Felix Andrews <felix@nfrac.org> 
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




### the code here deals with interacting (via grid viewports) with a
### lattice plot after it is made




## utility used in panel.identify

getTextPosition <- function(x, y)
    ## returns position 1: below, 2: left, 3: above, 4: right (w.r.t
    ## origin).  Used as a tool in panel.identify.
{
    a <- abs(c(x, y))
    if (y <= 0 && a[1] <= -y) 1
    else if (x <= 0 && a[2] <= -x) 2
    else if (y >= 0 && a[1] <= y) 3
    else if (x >= 0 && a[2] <= x) 4
}



panel.identify <-
    function(x, y = NULL,
             subscripts = seq_along(x),
             labels = subscripts, 
             n = length(x), offset = 0.5,
             threshold = 18, ## in points, roughly 0.25 inches
             panel.args = trellis.panelArgs(),
             ...)
    ## ... goes to ltext
{
    if (missing(x))
    {
        x <- panel.args$x
        y <- panel.args$y
        if (missing(subscripts) && !is.null(panel.args$subscripts))
            subscripts <- panel.args$subscripts
    }
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    labels <- as.character(labels)

    unmarked <- rep(TRUE, length(x))
    count <- 0

    while (count < n)
    {
        ll <- grid.locator(unit = "points")
        if (is.null(ll)) break ## non-left click
        lx <- convertX(ll$x, "points", TRUE)
        ly <- convertY(ll$y, "points", TRUE)
        pdists <- sqrt((px - lx)^2 + (py - ly)^2)
        if (min(pdists, na.rm = TRUE) > threshold)
            warning("no observations within ", threshold, " points")
        else
        {
            w <- which.min(pdists)
            if (unmarked[w])
            {
                pos <- getTextPosition(x = lx - px[w], y = ly - py[w])
                ltext(x[w], y[w], labels[w], pos = pos, offset = offset, ...)
                unmarked[w] <- FALSE
                count <- count + 1
            }
            else
                warning("nearest observation already identified")
        }
    }
    subscripts[!unmarked]
}




trellis.vpname <-
    function(name =
             c("position", "split", "split.location", "toplevel",
               "panel", "strip", "strip.left", "legend", "main", "sub",
               "xlab", "ylab", "page"),
             column = lattice.getStatus("current.focus.column"),
             row = lattice.getStatus("current.focus.row"),
             side = c("left", "top", "right", "bottom", "inside"),
             clip.off = FALSE,
             prefix = lattice.getStatus("current.prefix"))
{
    name <- match.arg(name)
    side <- match.arg(side)

    paste(prefix, 
          switch(name,

                 position = "position.vp",
                 split = "split.vp",
                 split.location = "split.location.vp",
                 toplevel = "toplevel.vp",

                 xlab = "xlab.vp",
                 ylab = "ylab.vp",
                 main = "main.vp",
                 sub  = "sub.vp",

                 panel =
                 if (clip.off) paste("panel", column, row, "off", "vp",  sep = ".")
                 else paste("panel", column, row, "vp", sep = "."), 

                 strip =
                 if (clip.off) paste("strip", column, row, "off", "vp", sep = ".")
                 else paste("strip", column, row, "vp", sep = "."), 

                 strip.left =
                 if (clip.off) paste("strip.left", column, row, "off", "vp", sep = ".")
                 else paste("strip.left", column, row, "vp", sep = "."), 

                 legend = paste("legend", side, "vp", sep = ".")),
          sep = ".")
}



trellis.grobname <-
    function(name, prefix = lattice.getStatus("current.prefix"))
{
    paste(prefix, name, sep = ".")
}


trellis.focus <-
    function(name,
             column = stop("column must be specified"),
             row = stop("row must be specified"),
             side = NULL,
             clip.off = FALSE,
             highlight = interactive(),
             ...)
{
    if (missing(name) && missing(column) && missing(row))
        return(trellis.clickFocus(clip.off = clip.off,
                                  highlight = highlight,
                                  ...))

    trellis.unfocus()

    if (name %in% c("panel", "strip", "strip.left"))
    {
        ll <- lattice.getStatus("current.panel.positions")
        if (column > 0 && row > 0 &&
            column <= ncol(ll) && row <= nrow(ll) &&
            ll[row, column] > 0) ## to disallow empty positions
        {
            lattice.setStatus(current.focus.column = column,
                              current.focus.row = row)
        }
        else
            stop("panel position unspecified or invalid")
    }
    else ## this is for calls from trellis.switchFocus
    {
        if (!missing(row)) lattice.setStatus(current.focus.row = row)
        if (!missing(column)) lattice.setStatus(current.focus.column = column)
    }
    lattice.setStatus(vp.depth = downViewport(trellis.vpname(name,
                      side = side, clip.off = clip.off)))
    if (highlight)
    {
        lattice.setStatus(vp.highlighted = TRUE)
        gp <- do.call("gpar",
                      updateList(lattice.getOption("highlight.gpar"),
                                 list(...)))
        lvp <- rectGrob(name = "lvp.highlight", gp = gp)
        grid.draw(lvp)
    }
    else
    {
        lattice.setStatus(vp.highlighted = FALSE)
    }
    invisible()
}



trellis.switchFocus <-
    function(name,
             side = NULL,
             clip.off = FALSE,
             highlight,
             ...)
{
    row <- lattice.getStatus("current.focus.row")
    column <- lattice.getStatus("current.focus.column")
    if (missing(highlight)) highlight <- lattice.getStatus("vp.highlighted")

    ## have to evaluate these explicitly to avoid lazy evaluation
    ## inside trellis.focus

    trellis.focus(name = name,
                  row = row,
                  column = column,
                  side = side, clip.off = clip.off,
                  highlight = highlight,
                  ...)
}



trellis.unfocus <-
    function()
    ## mainly, undo highlighting
{
    if (lattice.getStatus("vp.highlighted"))
    {
        grid.remove("lvp.highlight", warn = FALSE)
        lattice.setStatus(vp.highlighted = FALSE)
    }
    lattice.setStatus(current.focus.column = 0,
                      current.focus.row = 0)
    if (lattice.getStatus("vp.depth") > 0)
        upViewport(lattice.getStatus("vp.depth"))
    lattice.setStatus(vp.depth = 0)
    invisible()
}


### This version didn't work

## trellis.panelArgs <-
##     function(x, packet.number)
## {
##     if (lattice.getStatus("current.plot.multipage"))
##         warning("plot spans multiple pages, only last page can be updated")
##     if (missing(x)) 
##         if (lattice.getStatus("current.plot.saved")) x <- trellis.last.object()
##         else stop("current plot was not saved, can't retrieve panel data")
##     if (missing(packet.number))
##         packet.number <- packet.number()
##     if (!length(packet.number)) ## should be 0x0 matrix otherwise
##         stop("you have to first select a panel using trellis.focus()")
##     c(x$panel.args[[packet.number]], x$panel.args.common)
## }



trellis.panelArgs <-
    function(x, packet.number)
{
    if (lattice.getStatus("current.plot.multipage"))
        warning("plot spans multiple pages, only last page can be updated")
    if (missing(x)) 
        if (lattice.getStatus("current.plot.saved")) x <- trellis.last.object()
        else stop("current plot was not saved, can't retrieve panel data")
    if (missing(packet.number))
    {
        ## FIXME: workaround for unfortunate choice of names.  May
        ## require more extensive changes

        pn <- get("packet.number", mode = "function")
        packet.number <- pn()
    }
    if (!length(packet.number)) ## should be 0x0 matrix otherwise
        stop("you have to first select a panel using trellis.focus()")
    c(x$panel.args[[packet.number]], x$panel.args.common)
}



### based on an original version contributed by
### Felix Andrews <felix@nfrac.org> (2007/06/21)

trellis.clickFocus <-
    function(clip.off = FALSE,
             highlight = interactive(),
             ...)
{
    layoutMatrix <- trellis.currentLayout()
    trellis.focus("toplevel", highlight = FALSE)
    glayout <- lattice.getStatus("layout.details")
    rowRange <- range(glayout$pos.heights$panel, glayout$pos.heights$strip)
    colRange <- range(glayout$pos.widths$panel, glayout$pos.widths$strip.left)
    layCols <-  glayout$page.layout$ncol
    layRows <- glayout$page.layout$nrow
    leftPad <- convertX(sum(glayout$page.layout$widths[1:(colRange[1]-1)]), "npc", valueOnly = TRUE)
    rightPad <- convertX(sum(glayout$page.layout$widths[(colRange[2]+1):layCols]), "npc", valueOnly = TRUE)
    topPad <- convertY(sum(glayout$page.layout$heights[1:(rowRange[1]-1)]), "npc", valueOnly = TRUE)
    botPad <- convertY(sum(glayout$page.layout$heights[(rowRange[2]+1):layRows]), "npc", valueOnly = TRUE)
    clickLoc <- grid.locator("npc")
    if (is.null(clickLoc)) return()
    clickXScaled <- (as.numeric(clickLoc$x) - leftPad) / (1 - leftPad - rightPad)
    focusCol <- ceiling(clickXScaled * ncol(layoutMatrix))
    clickYScaled <- (as.numeric(clickLoc$y) - botPad) / (1 - botPad - topPad)
    focusRow <- ceiling(clickYScaled * nrow(layoutMatrix))
    if ((focusCol >= 1) && (focusCol <= ncol(layoutMatrix)) &&
        (focusRow >= 1) && (focusRow <= nrow(layoutMatrix)) &&
        layoutMatrix[focusRow, focusCol] > 0)
    {
        trellis.focus("panel", column = focusCol, row = focusRow,
                      clip.off = clip.off, highlight = highlight,
                      ...)
    }
    invisible(list(col=focusCol, row=focusRow))
}





## trellis.clickFocus <- function() {
##        layoutMatrix <- trellis.currentLayout()
##        currVpp <- current.vpPath()
##        if (!is.null(currVpp)) { upViewport(currVpp$n) }
##        depth <- downViewport(trellis.vpname("panel", 1, 1))
##        colRange <- current.viewport()$layout.pos.col[1]
##        rowRange <- current.viewport()$layout.pos.row[1]
##        upViewport()
##        downViewport(trellis.vpname("panel", ncol(layoutMatrix), nrow(layoutMatrix)))
##        colRange[2] <- current.viewport()$layout.pos.col[1]
##        rowRange[2] <- current.viewport()$layout.pos.row[1]
##        upViewport()
##        layCols <- current.viewport()$layout$ncol
##        layRows <- current.viewport()$layout$nrow
##        leftPad <- sum(sapply(current.viewport()$layout$widths[1:(min(colRange)-1)], convertX, "npc"))
##        rightPad <- sum(sapply(current.viewport()$layout$widths[(max(colRange)+1):layCols], convertX, "npc"))
##        topPad <- sum(sapply(current.viewport()$layout$heights[1:(min(rowRange)-1)],convertY, "npc"))
##        botPad <- sum(sapply(current.viewport()$layout$heights[(max(rowRange)+1):layRows],convertY, "npc"))
##        clickLoc <- grid.locator("npc")
##        # reset current viewport so lattice doesn't get confused
##        upViewport(depth-1)
##        if (!is.null(currVpp)) { downViewport(currVpp) }
##        if (is.null(clickLoc)) {
##                return(NULL)
##        }
##        clickXScaled <- (as.numeric(clickLoc$x) - leftPad) / (1 - leftPad - rightPad)
##        focusCol <- ceiling(clickXScaled * ncol(layoutMatrix))
##        clickYScaled <- (as.numeric(clickLoc$y) - botPad) / (1 - botPad - topPad)
##        focusRow <- ceiling(clickYScaled * nrow(layoutMatrix))
##        if ((focusCol < 1) || (focusCol > ncol(layoutMatrix))
##         || (focusRow < 1) || (focusRow > nrow(layoutMatrix))) {
##                focusCol <- focusRow <- 0
##                trellis.unfocus()
##        } else {
##                trellis.focus("panel", focusCol, focusRow)
##        }
##        invisible(list(col=focusCol, row=focusRow))
## }



