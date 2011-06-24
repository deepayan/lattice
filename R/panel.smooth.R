
### Copyright (C) 2001-2011  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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

## previously part of panels.R (r676)


panel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             lwd = plot.line$lwd,
             lty = plot.line$lty,
             col,
             col.line = plot.line$col,
             type, ## ignored
             horizontal = FALSE,
             ...,
             identifier = "loess")
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return()

    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal)
    {
        smooth <-
            loess.smooth(y[ok], x[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$y, y = smooth$x,
                    col = col.line, lty = lty, lwd = lwd, ...,
                    identifier = identifier)
    }
    else
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$x, y = smooth$y,
                    col = col.line, lty = lty, lwd = lwd, ...,
                    identifier = identifier)
    }
}



prepanel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) > 0)
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        list(xlim = range(x, smooth$x, finite = TRUE),
             ylim = range(y, smooth$y, finite = TRUE),
             dx = diff(smooth$x),
             dy = diff(smooth$y))
    }
    else prepanel.null()
}



# panel.smooth <-
#     function(x, y, span = 2/3, degree = 1, zero.line = FALSE,
#              family = c("symmetric", "gaussian"),
#              evaluation = 50,
#              lwd = plot.line$lwd, lty = plot.line$lty,
#              col = plot.line$col, ...)
# {
#     if (zero.line) abline(h=0, ...)
#     panel.loess(x, y, span = span, family = family,
#                 degree = degree, evaluation = evaluation, ...)
#     panel.xyplot(x, y, ...)
# }
## base R function exists



## panel.loess <-
##     function(x, y, span = 2/3, degree = 1,
##              family = c("symmetric", "gaussian"),
##              evaluation = 50,
##              lwd = plot.line$lwd,
##              lty = plot.line$lty,
##              col,
##              col.line = plot.line$col,
##              type, ## ignored
##              horizontal = FALSE,
##              ...,
##              identifier = "loess")



panel.spline <-
    function(x, y, npoints = 101,
             lwd = plot.line$lwd,
             lty = plot.line$lty,
             col, col.line = plot.line$col,
             type, horizontal = FALSE, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return()
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    spline.args <- as.list(formals(smooth.spline))
    new.args <- list(...)
    matched <- names(new.args)[names(new.args) %in% names(spline.args)]
    if (length(matched)) spline.args[matched] <- new.args[matched]
    if (horizontal)
    {
        spline.args$x <- y[ok]
        spline.args$y <- x[ok]
        fit <- do.call("smooth.spline",spline.args)
        yy <- seq(min(y[ok]),max(y[ok]), len=npoints)
        p <- predict(fit, x=yy)
        panel.lines(x=p$y, y=p$x, col=col.line, lty=lty, lwd=lwd, ...)
    }
    else
    {
        spline.args$x <- x[ok]
        spline.args$y <- y[ok]
        fit <- do.call("smooth.spline",spline.args)
        xx <- seq(min(x[ok]),max(x[ok]), len=npoints)
        p <- predict(fit, x=xx)
        panel.lines(x=p$x, y = p$y, col=col.line, lty=lty, lwd=lwd, ...)
    }
}




