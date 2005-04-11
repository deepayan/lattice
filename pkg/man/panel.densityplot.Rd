\name{panel.densityplot}
\alias{panel.densityplot}
\title{ Default Panel Function for densityplot }
\description{
  This is the default panel function for \code{densityplot}.
}
\usage{
panel.densityplot(x, darg, plot.points = TRUE, ref = FALSE,
                  col, col.line, ...)
}
\arguments{
  \item{x}{ data points for which density is to be estimated}
  \item{darg}{list of arguments to be passed to the \code{density}
    function. Typically, this should be a list with zero or more of the
    following components : \code{bw, adjust, kernel, window, width,
    give.Rkern, n, from, to, cut, na.rm} (see \code{density} for
    details)
} 
  \item{plot.points}{ logical, specifying whether or not the data 
    points should be plotted along with the estimated density.
  }
  \item{ref}{ logical, whether to draw x-axis }
  \item{col}{ color for points and line}
  \item{col.line}{ color for line, overrides col }
  \item{\dots}{ extra grahical parameters }
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{densityplot}}}
\keyword{dplot}
















