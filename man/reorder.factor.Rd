\name{reorderFactor}
\alias{reorderFactor}
\title{Reorder levels of a factor}
\description{
  Reorders the levels of a factor (or vector) depending on values of a
  second variable, usually numeric. The most common use is to reorder
  levels of a factor to be used in a plot in some visually meaningful
  way, by default in increasing order of the mean of the second variable
  grouped by the factor.
}

\usage{
reorderFactor(x, X, FUN = mean, \dots,
              order = is.ordered(x))
}
\arguments{
  \item{x}{ a vector from which a factor will be created, or a
    factor (possibly ordered) whose levels will be reordered
  }
  \item{X}{ a vector of the same length as \code{x}, whose subset
    of values for each unique level of \code{x} determines the
    eventual order of that level
  }
  \item{FUN}{ a function whose first argument is a vector and
    returns a scalar, to be applied to each subset of \code{X}
    determined by the levels of \code{x}
  }
  \item{\dots}{ extra arguments supplied to \code{FUN}}
  \item{order}{ logical, whether return value will be an ordered factor
    rather than a factor
  }
}

\value{
  A factor or an ordered factor (depending on the value of
  \code{order}), with the order of the levels determined by
  \code{FUN} applied to \code{X} grouped by \code{x}. The
  levels are ordered such that the values returned by \code{FUN}
  are in increasing order.
}

\note{
  This function is intended to replace the S-PLUS function
  \code{reorder.factor}, but differs from the S-PLUS version, which
  always returns an ordered factor.  It's functionality may be replaced
  by a \code{reorder} method for factors without notice, so it's not
  safe to rely on the existence of this function.
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{xyplot}}, \code{\link{Lattice}}}
\keyword{utilities}

