\name{rfs}
\alias{rfs}
\title{ Residual and Fit Spread Plots}
\description{
  Plots fitted values and residuals (via qqmath) on a common scale for
  any object that has methods for fitted values and residuals. 
}
\synopsis{
rfs(model, layout=c(2, 1), xlab="f-value", ylab=NULL,
    distribution = qunif,
    panel, prepanel, strip, ...)
}
\usage{
rfs(model, layout=c(2, 1), xlab="f-value", distribution = qunif,...)
}
\arguments{
  \item{model}{a fitted model object with methods \code{fitted.values}
    and \code{residuals}. Can be the value returned by \code{oneway} }
  \item{layout}{default layout is c(2,1)  }
  \item{xlab}{defaults to \code{"f.value"} }
  \item{distribution}{the distribution function to be used for \code{qqmath}}
  \item{\dots}{other arguments}
}
\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{oneway}}, \code{\link{qqmath}},
  \code{\link{xyplot}}, \code{\link{Lattice}}
}
\examples{
data(singer)
rfs(oneway(height ~ voice.part, data = singer, spread = 1), aspect = 1)
}
\keyword{dplot}
