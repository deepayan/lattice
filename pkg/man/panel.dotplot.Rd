\name{panel.dotplot}
\alias{panel.dotplot}
\title{ Default Panel Function for dotplot}
\description{
  Default panel function for \code{dotplot}.
}
\usage{
panel.dotplot(x, y, horizontal = TRUE,
              pch, col, lty, lwd, col.line,
              levels.fos = NULL, groups = NULL,
              \dots)
}
\arguments{
  \item{x,y}{ variables to be plotted in the panel. Typically y is the
    'factor' }
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{pch, col, lty, lwd, col.line}{ graphical parameters}
  \item{levels.fos}{ number of levels of \code{y}}
  \item{groups}{ grouping variable (affects graphical parameters)}
  \item{\dots}{ extra parameters, passed to \code{panel.xyplot} which is
    responsible for drawing the foreground points (\code{panel.dotplot}
    only draws the background reference lines).
  }
}
\details{ Creates (possibly grouped) Dotplot of \code{x} against
  \code{y}
}
\seealso{\code{\link{dotplot}}}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















