\name{panel.xyplot}
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
panel.xyplot(x, y, type="p",
             pch = plot.symbol$pch,
             col,
             col.line = plot.line$col,
             col.symbol = plot.symbol$col,
             font = plot.symbol$font,
             fontfamily = plot.symbol$fontfamily,
             fontface = plot.symbol$fontface,
             lty = plot.line$lty,
             cex = plot.symbol$cex,
             lwd = plot.line$lwd,
             horizontal = FALSE, ...)
panel.splom(...)
panel.qq(...)
}
\arguments{
  \item{x,y}{ variables to be plotted in the scatterplot}
  \item{type}{ character vector consisting of one or more of the
    following: \code{"p"}, \code{"l"}, \code{"h"}, \code{"b"},
    \code{"o"}, \code{"s"}, \code{"S"}, \code{"r"}, \code{"a"},
    \code{"g"}, \code{"smooth"}.  If type has more than one element, an
    attempt is made to combine the effect of each of the components.

    The behaviour if any of the first six are included in \code{type} is
    similar to the effect of \code{type} in \code{\link{plot}} (type
    \code{"b"} is actually the same as \code{"o"}).  \code{"r"} adds a
    regression line (same as \code{\link{panel.lmline}}, except for
    default graphical parameters), and \code{"smooth"} adds a lowess fit
    (same as \code{\link{panel.loess}}).  \code{"g"} adds a reference
    grid using \code{\link{panel.grid}} in the background.  \code{"a"}
    has the effect of calling \code{\link{panel.linejoin}}, which can be
    useful for creating interaction plots.  The effect of several of
    these specifications depend on the value of \code{horizontal}.

    See \code{example(xyplot)} and \code{demo(lattice)} for examples.
  }
  \item{col, col.line, col.symbol}{
    default colours are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}}.
  }
  \item{font, fontface, fontfamily}{
    font used when \code{pch} is a character
  }
  \item{pch, lty, cex, lwd}{
    other graphical parameters.
  }
  \item{\dots}{ extra arguments, if any, for \code{panel.xyplot}. In
    most cases \code{panel.xyplot} ignores these. For types "r" and
    "smooth", these are passed on to \code{panel.lmline} and
    \code{panel.loess} respectively.
  }
  \item{horizontal}{ logical. Controls orientation for certain
    \code{type}'s, e.g.  one of \code{"h"}, \code{"s"} or \code{"S"}}. }


\details{

  Creates scatterplot of \code{x} and \code{y}, with various
  modifications possible via the type argument. \code{panel.qq} draws a
  45 degree line before calling \code{panel.xyplot}.

  Note that most of the arguments controlling the display can be
  supplied directly to the high-level (e.g. \code{\link{xyplot}}) call.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\seealso{
  \code{\link{panel.superpose}},
  \code{\link{xyplot}},
  \code{\link{splom}}
}
\keyword{dplot}
