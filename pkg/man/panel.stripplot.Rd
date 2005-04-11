\name{panel.stripplot}
\alias{panel.stripplot}
\title{ Default Panel Function for stripplot }
\description{
  This is the default panel function for \code{stripplot}. Also see
  \code{panel.superpose}
}
\usage{
panel.stripplot(x, y, jitter.data = FALSE, factor = 0.5,
                horizontal = TRUE, groups = NULL,
                \dots)
}
\arguments{
  \item{x,y}{ coordinates of points to be plotted}
  \item{jitter.data}{ whether points should be jittered to avoid
    overplotting.} 
  \item{factor}{factor by which to jitter}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{groups}{ optional grouping variable }
  \item{\dots}{ graphical parameters etc can be supplied, passed to
    \code{panel.xyplot} or \code{panel.superpose}, depending on whether
    \code{groups} is present}
}
\details{ Creates stripplot (one dimensional scatterplot) of \code{x}
  for each level of \code{y}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{stripplot}}}
\keyword{dplot}
















