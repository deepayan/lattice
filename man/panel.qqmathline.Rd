\name{panel.qqmathline}
\alias{panel.qqmathline}
\title{Useful panel function with qqmath}
\description{
  Useful panel function with qqmath. Draws a line passing through the
  points determined by the .25 and .75 quantiles of the sample and the
  theoretical distribution.
}
\usage{
panel.qqmathline(y, distribution, ...)
}
\arguments{
  \item{y}{ The original sample, possibly reduced to a fewer number of
    quantiles, as determined by the \code{f.value} argument to
    \code{qqmath}
  }
  \item{distribution}{quantile function for reference theoretical
    distribution.
  }
  \item{\dots}{other arguments.}
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{prepanel.qqmathline}}, \code{\link{qqmath}}}
\keyword{dplot}
