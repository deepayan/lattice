

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
    function(x, y = NULL, labels = seq(along = x), 
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
    }
    xy <- xy.coords(x, y)
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
            warning("no points within ", threshold, " points")
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
                warning("nearest point already identified")
        }
    }
}





trellis.vpname <-
    function(name =
             c("position", "split", "split.location", "toplevel",
               "panel", "strip", "legend", "main", "sub",
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
    trellis.unfocus()

    if (name == "panel" || name == "strip")
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
    lattice.setStatus(vp.depth = downViewport(trellis.vpname(name, side = side, clip.off = clip.off)))
    if (highlight)
    {
        lattice.setStatus(vp.highlighted = TRUE)
        gp <- do.call("gpar", updateList(lattice.getOption("highlight.gpar"), list(...)))
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
    upViewport(lattice.getStatus("vp.depth"))
    lattice.setStatus(vp.depth = 0)
    invisible()
}




trellis.panelArgs <-
    function(x, panel.number)
{
    if (lattice.getStatus("current.plot.multipage"))
        warning("plot spans multiple pages, only last page can be updated")
    if (missing(x)) 
        if (lattice.getStatus("current.plot.saved")) x <- trellis.last.object()
        else stop("current plot was not saved, can't retrieve panel data")
    if (missing(panel.number))
    {
        row <- lattice.getStatus("current.focus.row")
        column <- lattice.getStatus("current.focus.column")
        if (row == 0 || column == 0)
            stop("you have to first select a panel using trellis.focus()")
        panel.number <- lattice.getStatus("current.panel.positions")[row, column]
    }
    c(x$panel.args[[panel.number]],
      x$panel.args.common,
      list(panel.number = panel.number))
}

