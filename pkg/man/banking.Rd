\name{banking}
\alias{banking}
\title{Banking}
\description{
  Calculates banking slope
}
\usage{
banking(dx, dy)
}
\arguments{
  \item{dx, dy}{ vector of consecutive x, y differences. }
}

\details{
  \code{banking} is a crude banking function used when
  \code{aspect = "xy"} in high level Trellis functions. It is usually not
  very meaningful except with \code{xyplot}.  It just considers the
  absolute slopes (based on \code{dx} and \code{dy}) and returns a value
  which when adjusted by the panel scale limits will make the median of
  the above absolute slopes correspond to a 45 degree line.

  This function was inspired by the discussion of banking in the
  documentation for Trellis Graphics available at Bell Labs' website
  (see \code{\link{Lattice}}), but is most likely identical to an
  algorithm described previously by Cleveland et al (see below).  It is
  not clear (to the author) whether this is the algorithm used in
  S-PLUS.  Alternative banking rules, implemented as a similar function,
  can be used as a drop-in replacement by suitably modifying
  \code{lattice.options("banking")}.

}


\references{

  Cleveland, William S.,  McGill, Marylyn E. and McGill, Robert (1988),
    "The Shape Parameter of a Two-variable Graph",
    \emph{Journal of the American Statistical Association}, 83, 289-300
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{Lattice}}, \code{\link{xyplot}}}
\keyword{dplot}












