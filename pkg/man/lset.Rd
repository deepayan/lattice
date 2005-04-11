\name{lset}
\alias{lset}
\title{Interface to modify Trellis Settings - Deprecated}
\description{
  A (hopefully) simpler alternative to \code{trellis.par.get/set}.  This
  is deprecated, and the same functionality is now available with
  \code{trellis.par.set}
}

\usage{
lset(theme = col.whitebg())
}
\arguments{
  \item{theme}{ a list decribing how to change the settings of the
    current active device. Valid components are those in the list
    returned by \code{trellis.par.get()}. Each component must itself be
    a list, with one or more of the appropriate components (need not
    have all components). Changes are made to the settings for the
    currently active device only.
  }
}
\details{
  \code{lset} is a wrapper to \code{trellis.par.set} that allows setting
  more than one parameter at a time, and only the components that are to
  be changed need be specified.
  
  This function is expected to be used in conjunction with functions
  that return `themes', i.e., lists which when supplied to \code{lset}
  change the overall look and feel of the display.

  \code{lset} is used to modify the settings on a one-time basis. It is
  also possible to make persistent changes in the default theme used via
  a user-settable option, see \code{\link{trellis.device}} for details.

}

\seealso{

  \code{\link{trellis.device}}, \code{\link{show.settings}},
  \code{\link{Lattice}}

}


\examples{
show.settings(col.whitebg())
# The following reproduces S-PLUS Unix black and white pch:
#lset(list(superpose.symbol=list(pch=c("o","+",">","s","w","#","\{"))))
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{utilities}
