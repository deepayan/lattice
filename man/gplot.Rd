\name{gplot}
\alias{gplot}
\alias{gplot.default}
\alias{gplotArgs}
\alias{gplotArgs.default}
\title{Grouped Display}
\description{
  Generic plotting function to produce grouped display using lattice
}
\usage{
gplot(x, ...)
gplot.default(x, ...)
gplotArgs(x, ...)
gplotArgs.default(x, ...)
gplotArgs.data.frame(x, display.formula, outer = FALSE,
                     groups = NULL, \dots, subset = TRUE)
}
\arguments{
  \item{\dots}{ extra arguments, passed on as appropriate  }
}
\details{


}

\value{
  \code{gplot} should produce an object of class ``trellis''. The
  `update' method can be used to update components of the object and the
  `print' method (usually called by default) will plot it on an
  appropriate plotting device.

  \code{gplotArgs} should return a list.

}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{Lattice}}
}
\examples{
gplot(iris)
}
\keyword{dplot}
