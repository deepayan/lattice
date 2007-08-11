\name{level.colors}
\alias{level.colors}
\title{
  A function to compute false colors representing a numeric or
  categorical variable
}
\description{
  Calculates false colors from a numeric variable (including factors,
  using their numeric codes) given a color scheme and breakpoints.
}
\usage{
level.colors(x, at, col.regions, colors = TRUE, ...)
}
\arguments{
  \item{x}{ A numeric or \code{\link{factor}} variable }
  \item{at}{ A numeric variable of breakpoints where colors change }
  \item{col.regions}{ A specification of the colors to be used.  This
    could be either a vector of colors, or a function that produces a
    vector of colors when called with a single argument giving the
    number of colors. }
  \item{colors}{ logical indicating whether colors should be computed
    and returned.  If \code{FALSE}, only the indices representing which
    interval (among those defined by \code{at}) each value in \code{x}
    falls into is returned.  }
  \item{\dots}{ Extra arguments, ignored. }
}
\value{
  A vector of the same length as \code{x}.  Depending on the
  \code{colors} argument, this could be either a vector of colors (in
  a form usable by \R), or a vector of integer indices representing
  which interval the values of \code{x} fall in.
}

\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org} }

\seealso{ \code{\link{levelplot}} }

\examples{

depth.col <-
    with(quakes, 
         level.colors(depth, at = do.breaks(range(depth), 30),
                      col.regions = terrain.colors))


xyplot(lat ~ long | equal.count(stations), quakes,
       strip = strip.custom(var.name = "Stations"),
       colours = depth.col,
       panel = function(x, y, colours, subscripts, ...) {
           panel.xyplot(x, y, pch = 21, col = "transparent",
                        fill = colours[subscripts], ...)
       })

}

\keyword{utilities}
