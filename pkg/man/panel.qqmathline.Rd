\name{panel.qqmathline}
\alias{panel.qqmathline}
\title{Useful panel function with qqmath}
\description{
  Useful panel function with qqmath. Draws a line passing through the
  points (usually) determined by the .25 and .75 quantiles of the sample
  and the theoretical distribution.
}

\usage{
panel.qqmathline(y, distribution,
                 p = c(0.25, 0.75),
                 \dots)
}
\arguments{
  \item{y}{ The original sample, possibly reduced to a fewer number of
    quantiles, as determined by the \code{f.value} argument to
    \code{qqmath}
  }
  \item{distribution}{quantile function for reference theoretical
    distribution.
  }
  \item{p}{
    numeric vector of length two, representing probabilities.
    Corresponding quantile pairs define the line drawn.
  }
  \item{\dots}{other arguments.}
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{prepanel.qqmathline}}, \code{\link{qqmath}}}
\keyword{dplot}
