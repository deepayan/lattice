% $Id: shingles.Rd,v 1.14 2003/03/03 01:03:32 deepayan Exp $
\name{shingles}
\alias{equal.count}
\alias{plot.shingle}
\alias{print.shingle}
\alias{print.shingleLevel}
\alias{summary.shingle}
\alias{as.data.frame.shingle}
\alias{as.factorOrShingle}
\alias{as.shingle}
\alias{[.shingle}
\alias{is.shingle}
\alias{shingle}
\title{shingles}
\usage{
shingle(x, intervals=sort(unique(x)))
equal.count(x, \dots)
as.shingle(x)
is.shingle(x)
\method{plot}{shingle}(x, col, aspect, \dots)
\method{print}{shingle}(x, showValues = TRUE, \dots)
\method{print}{shingleLevel}(x, \dots)
\method{summary}{shingle}(object, \dots)
\method{as.data.frame}{shingle}(x, row.names = NULL, optional = FALSE)
x[subset, drop = FALSE]
as.factorOrShingle(x, subset, drop)
}
\description{
  Functions to handle shingles
}
\arguments{
  \item{x}{
    numeric variable or R object, shingle in \code{plot.shingle} and
    \code{x[]}. An object (list of intervals) of class "shingleLevel" in
    \code{print.shingleLevel}
  }
  \item{object}{ shingle object to be summarized}
  \item{showValues}{ logical, whether to print the numeric part. If
    FALSE, only the intervals are printed}
  \item{row.names}{ a character vector giving the row names for the data frame}
  \item{optional}{ logical. If `TRUE', setting row names is optional}
  \item{intervals}{ numeric vector or matrix with 2 columns}
  \item{subset}{logical vector}
  \item{drop}{whether redundant shingle levels are to be dropped}
  \item{col}{ color to fill the rectangles, defaults to
    \code{bar.fill$col}} 
  \item{aspect}{aspect ratio}
  \item{\dots}{ other arguments, passed to \code{co.intervals}}
}
\details{
  A shingle is a data structure used in Trellis, and is meant to be a
  generalization of factors to `continuous' variables. It consists of a
  numeric vector along with some possibly overlapping intervals. These
  intervals are the `levels' of the shingle. The \code{levels} and
  \code{nlevels} functions, usually applicable to factors, are also
  applicable to shingles.

  There are print methods for shingles, as well as for printing the
  result of \code{levels()} applied to a shingle.

  The implementation of shingles is slightly different from S.

  \code{equal.count} converts \code{x} to a shingle. Essentially a
  wrapper around \code{co.intervals}. All arguments are passed to
  \code{co.intervals}

  \code{shingle} creates a shingle using the given \code{intervals}. If
  \code{intervels} is a vector, these are used to form 0 length
  intervals.

  \code{as.shingle} returns \code{shingle(x)} if \code{x} is not a
  shingle.

  \code{is.shingle} tests whether \code{x} is a shingle.

  \code{plot.shingle} displays the ranges of shingles via
  rectangles. \code{print.shingle} and \code{summary.shingle} describe
  the shingle object.
}
\value{
  \code{x$intervals} for \code{levels.shingle(x)}, 
  logical for \code{is.shingle}, an object of class ``trellis'' for
  \code{plot} (printed by default by \code{print.trellis}), and 
  an object of class ``shingle'' for the others.
}
\examples{
z <- equal.count(rnorm(50))
plot(z)
print(z)
print(levels(z))
\testonly{data.frame(x = equal.count(rnorm(100)), y = rnorm(100))}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{xyplot}},
  \code{\link{co.intervals}}, \code{\link{Lattice}} 
}
\keyword{print}
\keyword{hplot}
\keyword{methods}
\keyword{classes}



