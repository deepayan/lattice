\name{F_1_panel.qqmath}
\alias{panel.qqmath}
\title{ Default Panel and Prepanel Function for qqmath }
\description{
  This is the default panel function for \code{qqmath}.
}
\usage{
panel.qqmath(x, f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL, \dots)
}
\arguments{
  \item{x}{
    vector (typically numeric, coerced if not) of data values to be used
    in the panel.
  }
  \item{f.value, distribution}{
    Defines how quantiles are calculated.  See \code{\link{qqmath}} for
    details.
  }
  \item{qtype}{
    The \code{type} argument to be used in \code{\link{quantile}}
  }
  \item{groups}{
    An optional grouping variable.  Within each panel, one Q-Q plot is
    produced for every level of this grouping variable, differentiated
    by different graphical parameters.
  }
  \item{\dots}{
    further arguments, often graphical parameters.
  }
}

\details{

  Creates a Q-Q plot of the data and the theoretical distribution given
  by \code{distribution}.  Note that most of the arguments controlling
  the display can be supplied directly to the high-level \code{qqmath}
  call.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}

\seealso{
  \code{\link{qqmath}}
}
\keyword{dplot}
