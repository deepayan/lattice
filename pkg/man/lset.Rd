\name{lset}
\alias{lset}
\alias{col.whitebg}
\title{Simple Interfaces to modify Trellis Settings}
\description{
  A (hopefully) simpler alternative interface to
  \code{trellis.par.get/set}. NOT S compatible
}
\usage{
lset(theme = col.whitebg())
col.whitebg()
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

  \code{col.whitebg} is one such function, which returns a list which
  when supplied to \code{lset} changes the background to transparent and
  the other colors to something suitable. Contributions of more such
  `themes' are welcome. 

  \code{lset} is used to modify the settings on a one-time basis. It is
  also possible to make persistent changes in the default theme used via
  a user-settable option, see \code{\link{trellis.device}} for details.

}

\seealso{

  \code{\link{lattice.theme}}, \code{\link{trellis.device}},
  \code{\link{show.settings}}, \code{\link{Lattice}}

}


\examples{
show.settings(col.whitebg())
# The following reproduces S-PLUS Unix black and white pch:
#lset(list(superpose.symbol=list(pch=c("o","+",">","s","w","#","\{"))))
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{utilities}
