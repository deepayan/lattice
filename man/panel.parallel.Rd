\name{panel.parallel}
\alias{panel.parallel}
\title{ Default Panel Function for parallel}
\description{
  This is the default panel function for \code{parallel}.
}
\usage{
panel.parallel(z, subscripts,
               groups = NULL,
               col, lwd, lty, alpha,
               \dots)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{subscripts}{
    The indices of the rows of \code{z} that are to be displyed in this
    panel.
  }
  \item{groups}{
    An optional grouping variable.  If specified, different groups are
    distinguished by use of different graphical parameters (rows
    of \code{z} in the same group share parameters).
  }
  \item{col, lwd, lty, alpha}{
    graphical parameters (defaults to the settings for
    \code{superpose.line}).  If \code{groups} is non-null, these
    parameters used one for each group.  Otherwise, they are recycled
    and used to distinguish between rows of the data frame \code{z}.
  }
  \item{\dots}{ other arguments (ignored) }
}
\details{difficult to describe. See example for \code{parallel}}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{parallel}}}
\keyword{dplot}
















