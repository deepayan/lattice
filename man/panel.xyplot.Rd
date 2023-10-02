\name{F_1_panel.xyplot}
\alias{panel.xyplot}
\alias{panel.splom}
\alias{panel.qq}
\title{ Default Panel Function for xyplot }
\description{
  This is the default panel function for \code{xyplot}. Also see
  \code{panel.superpose}.  The default panel functions for
  \code{splom} and \code{qq} are essentially the same function.
}
\usage{
panel.xyplot(x, y, type = "p",
             groups = NULL,
             pch, col, col.line, col.symbol,
             font, fontfamily, fontface,
             lty, cex, fill, lwd,
             horizontal = FALSE, \dots,
             smooth = NULL,
             grid = lattice.getOption("default.args")$grid,
             abline = NULL,
             jitter.x = FALSE, jitter.y = FALSE,
             factor = 0.5, amount = NULL,
             identifier = "xyplot")
panel.splom(\dots, identifier = "splom")
panel.qq(\dots, identifier = "qq")
}
\arguments{
  \item{x,y}{ variables to be plotted in the scatterplot }

  \item{type}{ character vector controlling how \code{x} and \code{y}
    are to be plotted. Can consist of one or more of the following:
    \code{"p"}, \code{"l"}, \code{"h"}, \code{"b"}, \code{"o"},
    \code{"s"}, \code{"S"}, \code{"g"}, \code{"r"}, \code{"a"}, 
    \code{"smooth"}, and \code{"spline"}.  If \code{type} has more than
    one element, an attempt is made to combine the effect of each of the
    components.

    The behaviour if any of the first five are included in \code{type}
    is similar to the effect of the corresponding \code{type} in
    \code{\link{plot}}: \code{"p"} and \code{"l"} stand for points and
    lines respectively; \code{"b"} and \code{"o"} (for \sQuote{overlay})
    plot both; \code{"h"} draws vertical (or horizontal if
    \code{horizontal = TRUE}) line segments from the points to the
    origin.  Types \code{"s"} and \code{"S"} are like \code{"l"} in the
    sense that they join consecutive points, but instead of being joined
    by a straight line, points are connected by a vertical and a
    horizontal segment forming a \sQuote{step}, with the vertical
    segment coming first for \code{"s"}, and the horizontal segment
    coming first for \code{"S"}.  Types \code{"s"} and \code{"S"} sort
    the values along one of the axes (depending on \code{horizontal});
    this is unlike the behavior in \code{plot}. For the latter behavior,
    use \code{type = "s"} with \code{panel = panel.points}.

    Type \code{"g"} adds a reference grid using \code{\link{panel.grid}}
    in the background, but using the \code{grid} argument is now the
    preferred way to do so.

    The remaining values of \code{type} lead to various types of
    smoothing. This can also be achieved using the \code{smooth}
    argument, or by calling the relevant panel functions directly. The
    panel functions provide finer control over graphical and other
    parameters, but using \code{smooth} or \code{type} is convenient for
    simple usage. Using \code{smooth} is recommended, but \code{type} is
    also supported for backwards compatibility.

    Type \code{"r"} adds a linear regression line, \code{"smooth"} adds
    a loess fit, \code{"spline"} adds a cubic smoothing spline fit, and
    \code{"a"} draws line segments joining the average \code{y} value
    for each distinct \code{x} value. See \code{smooth} for details.

    See \code{example(xyplot)} and \code{demo(lattice)} for examples.
  }
  \item{groups}{ an optional grouping variable.  If present,
    \code{\link{panel.superpose}} will be used instead to display each
    subgroup }
  \item{col, col.line, col.symbol}{
    default colours are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}}.
  }
  \item{font, fontface, fontfamily}{
    font used when \code{pch} is a character
  }
  \item{pch, lty, cex, lwd, fill}{
    other graphical parameters.  \code{fill} serves the purpose of
    \code{bg} in \code{\link{points}} for certain values of \code{pch}
  }
  \item{horizontal}{
    A logical flag controlling the orientation for certain
    \code{type}'s, e.g., \code{"h"}, \code{"s"}, ans \code{"S"} and the
    result of smoothing.
  }
  \item{\dots}{
    Extra arguments, if any, for \code{panel.xyplot}.  Usually passed on
    as graphical parameters to low level plotting functions, or to the
    panel functions performing smoothing, if applicable.
  }
  \item{smooth}{ If specificied, indicates the type of smooth to be
    added. Can be a character vector containing one or more values from
    \code{"lm"}, \code{"loess"}, \code{"spline"}, and
    \code{"average"}. Can also be a logical flag; \code{TRUE} is
    interpreted as \code{"loess"}. Each of these result in calling a
    corresponding panel function as described below; the \code{smooth}
    argument simply provides a convenient shortcut.

    \code{"lm"} adds a linear regression line (same as
    \code{\link{panel.lmline}}, except for default graphical
    parameters). \code{"loess"} adds a loess fit (same as
    \code{\link{panel.loess}}).  \code{"spline"} adds a cubic smoothing
    spline fit (same as \code{\link{panel.spline}}).  \code{"average"}
    has the effect of calling \code{\link{panel.average}}, which in
    conjunction with a \code{groups} argument can be useful for creating
    interaction plots.

    Normally, smoothing is performed with the \code{y} variable as the
    response and the \code{x} variable as the predictor.  However, the
    roles of \code{x} and \code{y} are reversed if \code{horizontal =
    TRUE}.
  }

  \item{grid}{
    A logical flag, character string, or list specifying whether and how
    a background grid should be drawn.  This provides the same
    functionality as \code{type="g"}, but is the preferred alternative
    as the effect \code{type="g"} is conceptually different from that of
    other \code{type} values (which are all data-dependent).  Using the
    \code{grid} argument also allows more flexibility.

    Most generally, \code{grid} can be a list of arguments to be
    supplied to \code{\link{panel.grid}}, which is called with those
    arguments.  Three shortcuts are available:
    \describe{
      \item{\code{TRUE}:}{ roughly equivalent to \code{list(h = -1, v = -1)} }
      \item{\code{"h"}:}{ roughly equivalent to \code{list(h = -1, v = 0)} }
      \item{\code{"v"}:}{ roughly equivalent to \code{list(h = 0, v = -1)} }
    }
    No grid is drawn if \code{grid = FALSE}.
  }

  \item{abline}{
    A numeric vector or more generally a list containing arguments that
    are used to call \code{\link{panel.abline}}. If specified as a
    numeric vector, \code{abline} is used as the first unnamed argument
    to \code{\link{panel.abline}}.  This allows arguments of the form
    \code{abline = c(0, 1)}, which adds the diagonal line, or
    \code{abline = coef(fm)} to fit the regression line from a fitted
    mode. Use the list form for finer control; e.g., \code{abline =
    list(h = 0, v = 0, col = "grey")}.

    For more flexibility, use \code{\link{panel.abline}} directly.
  }
  \item{jitter.x, jitter.y}{ logical, whether the data should be
    jittered before being plotted. }
  \item{factor, amount}{ controls amount of jittering. }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}



\details{

  Creates scatterplot of \code{x} and \code{y}, with various
  modifications possible via the type argument. \code{panel.qq} draws a
  45 degree line before calling \code{panel.xyplot}.

  Note that most of the arguments controlling the display can be
  supplied directly to the high-level (e.g. \code{\link{xyplot}}) call.

}

\examples{

types.plain <- c("p", "l", "o", "r", "g", "s", "S", "h", "a", "smooth")
types.horiz <- c("s", "S", "h", "a", "smooth")
horiz <- rep(c(FALSE, TRUE), c(length(types.plain), length(types.horiz)))

types <- c(types.plain, types.horiz)

x <- sample(seq(-10, 10, length.out = 15), 30, TRUE)
y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)

xyplot(y ~ x | gl(1, length(types)),
       xlab = "type", 
       ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
       as.table = TRUE, layout = c(5, 3),
       between = list(y = c(0, 1)),
       strip = function(...) {
           panel.fill(trellis.par.get("strip.background")$col[1])
           type <- types[panel.number()]
           grid::grid.text(label = sprintf('"\%s"', type), 
                           x = 0.5, y = 0.5)
           grid::grid.rect()
       },
       scales = list(alternating = c(0, 2), tck = c(0, 0.7), draw = FALSE),
       par.settings = 
       list(layout.widths = list(strip.left = c(1, 0, 0, 0, 0))),
       panel = function(...) {
           type <- types[panel.number()]
           horizontal <- horiz[panel.number()]
           panel.xyplot(..., 
                        type = type,
                        horizontal = horizontal)
       })[rep(1, length(types))]

}


\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\seealso{
  \code{\link{panel.superpose}},
  \code{\link{xyplot}},
  \code{\link{splom}}
}
\keyword{dplot}
