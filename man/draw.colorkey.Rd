\name{D_draw.colorkey}
\alias{draw.colorkey}
\title{Produce a colorkey typically for levelplot}
\description{
  Creates (and optionally draws) a grid frame grob representing a color
  key that can be placed in other grid-based plots. Primarily used by
  \code{levelplot} when a color key is requested.
}
\usage{
draw.colorkey(key, draw = FALSE, vp = NULL)
}
\arguments{
  \item{key}{
    A list determining the key. See documentation for \code{levelplot},
    in particular the section describing the \code{colorkey} argument,
    for details.
  }
  \item{draw}{ A scalar logical, indicating whether the grob is to be drawn. }
  \item{vp}{ The viewport in which to draw the grob, if applicable. }
}
\value{
  A grid frame object (that inherits from \code{"grob"})
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{xyplot}}}
\keyword{dplot}

