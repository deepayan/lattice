

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


## FIXME: needs work
panel.identify <-
    function(x, y = NULL, labels = seq(along = x), 
             n = length(x), offset = 0.5,
             threshold = 18, ## in points, roughly 0.25 inches
             ...)
    ## ... goes to ltext
    ## is this interruptible?
    ## doesn't track points already identified
{
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    labels <- as.character(labels)
    for (i in seq(length = n))
    {
        ll <- grid.locator(unit = "points")
        if (convertX(ll$x, "npc", TRUE) < 0.0001) break ## need to do better
        lx <- convertX(ll$x, "points", TRUE)
        ly <- convertY(ll$y, "points", TRUE)
        pdists <- sqrt((px - lx)^2 + (py - ly)^2)
        if (min(pdists, na.rm = TRUE) > threshold)
            warning("no points within threshold of ", threshold, " points")
        else
        {
            w <- which.min(pdists)
            pos <- getTextPosition(x = lx - px[w], y = ly - py[w])
            ltext(x[w], y[w], labels[w], pos = pos, offset = offset, ...)
        }
    }
}



latticeVP.switch <-
    function(name = c("panel", "strip"))
{
    name <- match.arg(name)
    if (lattice.getStatus(vp.highlighted))
    {
        lvp <- grid.get("lvp.highlight")
        grid.remove("lvp.highlight")
        seekViewport(paste(name, column, row, sep = "."))
        grid.draw(lvp)
    }
    else
        seekViewport(paste(name, column, row, sep = "."))
    invisible()
}


## Not sure whether (or how) to allow addition of axes.


latticeVP.focus <-
    function(column, row, name = c("panel", "strip"),
             highlight = interactive(), 
             ...)
{
    name <- match.arg(name)
    latticeVP.exit()
    lattice.setStatus(current.focus.column = column,
                      current.focus.row = row)
    if (highlight)
    {
        lattice.setStatus(vp.highlighted = TRUE)
        gplist <- lattice.options("highlight.gpar")
        splist <- list(...)
        gp <- do.call("gpar", updateList(gplist, splist))
        lvp <- rectGrob(name = "lvp.highlight", gp = gp)
        seekViewport(paste(name, column, row, sep = "."))
        grid.draw(lvp)
    }
    else lattice.setStatus(vp.highlighted = FALSE)
    invisible()
}


latticeVP.unfocus <-
    function()
    ## mainly, undo highlighting
{
    if (lattice.getStatus(vp.highlighted))
        grid.remove("lvp.highlight")
    lattice.setStatus(current.focus.column = 0,
                      current.focus.row = 0)
    invisible()
}






latticeVP.panelArgs <-
    function()
    ## would work only if current object saved (and not multipage)
{
    stop("not implemented yet")
}

