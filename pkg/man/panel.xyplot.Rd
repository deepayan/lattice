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
panel.xyplot(x, y, type = "p",
             groups = NULL,
             pch, col, col.line, col.symbol,
             font, fontfamily, fontface,
             lty, cex, fill, lwd,
             horizontal = FALSE, \dots,
             jitter.x = FALSE, jitter.y = FALSE,
             factor = 0.5, amount = NULL)
panel.splom(\dots)
panel.qq(\dots)
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
  \item{\dots}{ extra arguments, if any, for \code{panel.xyplot}. In
    most cases \code{panel.xyplot} ignores these. For types "r" and
    "smooth", these are passed on to \code{panel.lmline} and
    \code{panel.loess} respectively.
  }
  \item{horizontal}{ logical. Controls orientation for certain
    \code{type}'s, e.g.  one of \code{"h"}, \code{"s"} or \code{"S"}}.
  \item{jitter.x, jitter.y}{ logical, whether the data should be
    jittered before being plotted. }
  \item{factor, amount}{ controls amount of jittering. }
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

x <- sample(seq(-10, 10, length = 15), 30, TRUE)
y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)

xyplot(y ~ x | gl(1, length(types)),
       xlab = "type", 
       ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
       as.table = TRUE, layout = c(5, 3),
       between = list(y = c(0, 1)),
       strip = function(...) {
           panel.fill(trellis.par.get("strip.background")$col[1])
           type <- types[panel.number()]
           grid::grid.text(lab = sprintf('"%s"', type), 
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
