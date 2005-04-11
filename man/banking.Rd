\name{banking}
\alias{banking}
\title{Banking}
\description{
  Calculates `banking' slope
}
\usage{
banking(dx, dy)
}
\arguments{
  \item{dx, dy}{ vector of consecutive x, y differences. }
}
\details{
  \code{banking} is a crude banking function used when aspect="xy" in
  high level Trellis functions. Its usually not very menaingful except
  for \code{xyplot}. It just orders the absolute slopes and returns a value
  which when adjusted by the panel scale limits will make the median of
  the above absolute slopes a 45 degree line.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{Lattice}}, \code{\link{xyplot}}}
\keyword{dplot}
















