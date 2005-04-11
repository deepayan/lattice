\name{lattice.options}
\alias{lattice.options}
\alias{lattice.getOption}
\title{ Low-level Options Controlling Behaviour of Lattice }
\description{
  Functions to handle settings used by lattice.  Their main purpose is
  to make code maintainance easier, and users normally should not need
  to use these functions.  However, fine control at this level maybe
  useful in certain cases.
}

\usage{
lattice.options(\dots)
lattice.getOption(name)
}
\arguments{
  \item{name}{ character giving the name of a setting }
  \item{\dots}{
    new options can be defined, or existing ones modified, using one or
    more arguments of the form \code{name = value} or by passing a list
    of such tagged values.  Existing values can be retrieved by
    supplying the names (as character strings) of the components as
    unnamed arguments.
  }
}
\details{
  These functions are modeled on \code{options} and \code{getOption},
  and behave similarly for the most part.  The components currently used
  are not documented here, but are fairly self-explanatory.
}

\value{
  \code{lattice.getOption} returns the value of a single component,
  whereas \code{lattice.options} always returns a list with one or more
  named components.  When changing the values of components, the old
  values of the modified components are returned by
  \code{lattice.options}.  If called without any arguments, the full
  list is returned. 
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\seealso{
  \code{\link{options}}, \code{\link{trellis.device}},
  \code{\link{trellis.par.get}}, \code{\link{Lattice}}
}

\examples{
str(lattice.options())
lattice.getOption("save.object")
}
\keyword{dplot}


