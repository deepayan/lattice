\name{simpleKey}
\alias{simpleKey}
\title{ Function to generate a simple key }
\description{
  Simple interface to generate a list appropriate for \code{draw.key}
}
\usage{
simpleKey(text, points = TRUE,
          rectangles = FALSE,
          lines = FALSE, \dots)
}
\arguments{
  \item{text}{ character or expression vector, giving levels of grouping
    variable
  }
  \item{points}{logical}
  \item{rectangles}{logical}
  \item{lines}{logical}
  \item{\dots}{ arguments added to the list, finally passed to \code{draw.key}}
}
\details{
  A lattice plot can include a legend (key) if an appropriate list is
  specified as the \code{key} argument to a high level Lattice function
  such as \code{xyplot}. This key can be very flexible, but that
  flexibility comes at the cost of this list being very complicated even
  in simple situations. The \code{simpleKey} function is a shortcut,
  which assumes that the key is being drawn in conjunction with the
  \code{groups} argument, and that the default Trellis settings are
  being used. At most one each of points, rectangles and lines can be
  drawn.

  See also the \code{auto.key} argument for high level plots.
}
\value{
  A list that would work as the \code{key} argument to \code{xyplot}
  etc.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{draw.key}}, \code{\link{xyplot}}, \code{\link{Lattice}}}

\keyword{dplot}
















