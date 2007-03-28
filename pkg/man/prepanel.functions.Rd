\name{prepanel.functions}
\alias{prepanel.lmline}
\alias{prepanel.loess}
\alias{prepanel.qqmathline}
\title{ Useful Prepanel Function for Lattice}
\description{
  These are predefined prepanel functions available in Lattice.
}

\usage{
prepanel.lmline(x, y, \dots)
prepanel.loess(x, y, span, degree, family, evaluation, \dots)
prepanel.qqmathline(x, y = x, distribution = qnorm,
                    p = c(0.25, 0.75), qtype = 7,
                    groups, subscripts,
                    \dots) 
}
\arguments{
  \item{x, y}{x and y values, numeric or factor}
  \item{distribution}{ quantile function for theoretical
    distribution. This is automatically passed in when this is used as a
    prepanel function in \code{qqmath}.
  }
  \item{qtype}{ type of \code{\link{quantile}}}
  \item{p}{
    numeric vector of length two, representing probabilities.  If used
    with \code{aspect="xy"}, the aspect ratio will be chosen to make the
    line passing through the corresponding quantile pairs as close to 45
    degrees as possible.
  }
  \item{span, degree, family, evaluation}{ arguments controlling the
    underlying \code{\link{loess}} smooth }
  \item{groups, subscripts}{ See \code{\link{xyplot}}.  Whenever
    appropriate, calculations are done separately for each group and
    then combined.  }
  \item{\dots}{ other arguments }
}
\value{
  usually a list with components \code{xlim}, \code{ylim}, \code{dx} and
  \code{dy}, the first two being used to calculate panel axes limits,
  the last two for banking computations.  The form of these components
  are described under \code{\link{xyplot}}.  There are also several
  prepanel functions that serve as the default for high level functions,
  see \code{\link{prepanel.default.xyplot}}
}

\seealso{

  \code{\link{trellis.par.get}}, \code{\link{xyplot}},
  \code{\link{banking}}, \code{\link{Lattice}}. See
  \code{\link{loess.smooth}} for further options to
  \code{prepanel.loess}

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

