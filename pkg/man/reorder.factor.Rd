\name{reorder.factor}
\alias{reorder.factor}
\title{Reorder levels of a factor}
\description{
  Reorders the levels of a factor (or vector) depending on values of a
  second variable, usually numeric. The most common use is to reorder
  levels of a factor to be used in a plot in some visually meaningful
  way, by default in increasing order of the mean of the second variable
  grouped by the factor.
}

\usage{
reorder.factor(Factor, X, Function = mean, \dots,
               order = is.ordered(Factor))
}
\arguments{
  \item{Factor}{ a vector from which a factor will be created, or a
    factor (possibly ordered) whose levels will be reordered
  }
  \item{X}{ a vector of the same length as \code{Factor}, whose subset
    of values for each unique level of \code{Factor} determines the
    eventual order of that level
  }
  \item{Function}{ a function whose first argument is a vector and
    returns a scalar, to be applied to each subset of \code{X}
    determined by the levels of \code{Factor}
  }
  \item{\dots}{ extra arguments supplied to \code{Function}}
  \item{order}{ logical, whether return value will be an ordered factor
    rather than a factor
  }
}

\value{
  A factor or an ordered factor (depending on the value of
  \code{order}), with the order of the levels determined by
  \code{Function} applied to \code{X} grouped by \code{Factor}. The
  levels are ordered such that the values returned by \code{Function}
  are in increasing order.
}

\note{
  This function differs from the S-PLUS version, which always returns an
  ordered factor.
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{xyplot}}, \code{\link{Lattice}}}
\keyword{utilities}

