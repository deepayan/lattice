

### Copyright 2001  Deepayan Sarkar <deepayan@stat.wisc.edu>
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



prepanel.default.tmd <-
    function(...)
    prepanel.default.xyplot(...)



panel.tmd <- function(...) {
    panel.abline(h=0)
    panel.xyplot(...)
}







tmd <- function(object,
                xlab = "mean",
                ylab = "difference",
                panel = "panel.tmd",
                prepanel = "prepanel.default.tmd",
                ...)
{

    ## data x, y are not always in panel.args (they may be in
    ## panel.args.common), but they are for xyplot and qq, which are
    ## all this is supposed to work for. May modify this if there's
    ## demand.

    for (panel.number in seq(along = object$panel.args))
    {
        p <- object$panel.args[[panel.number]]
        x <- (as.numeric(p$x) + as.numeric(p$y)) / 2
        y <- as.numeric(p$y) - as.numeric(p$x)

        object$panel.args[[panel.number]]$x <- x
        object$panel.args[[panel.number]]$y <- y

    }

    object$xlab.default <- "mean"
    object$ylab.default <- "difference"

    update(object,
           xlab = xlab, ylab = ylab,
           panel = panel,
           prepanel = prepanel,
           ...)
}
                

