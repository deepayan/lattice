\name{panel.histogram}
\alias{panel.histogram}
\title{ Default Panel Function for histogram }
\description{
  This is the default panel function for \code{histogram}.
}
\usage{
panel.histogram(x, breaks, equal.widths, type, col, ...)
}
\arguments{
  \item{x}{ The data points for which the histogram is to be drawn} 
  \item{breaks}{ The breakpoints for the histogram}
  \item{equal.widths}{ logical used when \code{breaks==NULL} }
  \item{type}{ Type of histogram, possible values being ``percent'',
    ``density'' and ``count''}
  \item{col}{ Color of bars (NULL for empty), by default
    \code{bar.fill$col}.}
  \item{\dots}{ other arguments.}
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{histogram}}}
\keyword{dplot}
















