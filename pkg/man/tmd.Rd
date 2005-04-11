\name{tmd}
\alias{tmd}
\title{Tukey Mean-Difference Plot}
\description{
  Create Tukey Mean-Difference Plots from a trellis object returned by
  \code{xyplot} and \code{qq}
}
\usage{
tmd(object,
    xlab = "mean",
    ylab = "difference",
    panel = "panel.tmd",
    prepanel = "prepanel.default.tmd",
    ...)
}
\arguments{
  \item{object}{ An object of class ``trellis'' returned by
    \code{xyplot} or \code{qq}
  }
  \item{xlab}{ x label}
  \item{ylab}{ y label}
  \item{panel}{ panel function to be used }
  \item{prepanel}{
    prepanel function used to recalculate limits using the modified
    \code{x} and \code{y} values
  }
  \item{\dots}{ other arguments}
}
\details{
  The Tukey Mean-difference plot is produced by modifying the (x,y)
  values of each panel as follows: the new coordinates are given by
  \code{x=(x+y)/2} and \code{y=y-x}.

  \code{tmd} uses the \code{update} method for ``trellis'' objects,
  which processes all extra arguments supplied to \code{tmd}.
}

\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{ \code{\link{panel.tmd}}, \code{\link{xyplot}},
  \code{\link{Lattice}}
}
\examples{
tmd(qqmath(~height|voice.part, data = singer))
}
\keyword{dplot}
