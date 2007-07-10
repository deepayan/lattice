\name{barchart.table}
\title{table methods for barchart and dotplot}
\alias{barchart.table}
\alias{barchart.array}
\alias{barchart.matrix}
\alias{dotplot.table}
\alias{dotplot.array}
\alias{dotplot.matrix}

\usage{
\method{barchart}{table}(x, data, groups = TRUE,
         origin = 0, stack = TRUE, \dots, horizontal = TRUE)

\method{barchart}{array}(x, data, \dots)

\method{barchart}{matrix}(x, data, \dots)

\method{dotplot}{table}(x, data, groups = TRUE, \dots)

\method{dotplot}{array}(x, data, \dots)

\method{dotplot}{matrix}(x, data, \dots)
}

\description{
  Contingency tables are often displayed using barcharts and dotplots.
  These methods are provided for convenience and operate directly on
  tables.  Arrays and matrices are simply coerced to be a table.
}

\arguments{
  \item{x}{
    a \code{table}, \code{array} or \code{matrix} object.
  }
  \item{data}{
    should not be specified. If specified, will be ignored with a
    warning.
  }
  \item{groups}{
    logical, whether to use the last dimension as the grouping variable
    in the display.
  }
  \item{origin, stack}{
    arguments to \code{panel.barchart} controlling the display.  The
    defaults for the \code{table} method are diffrent.
  }
  \item{horizontal}{ logical, indicating whether the bars should be
    horizontal or vertical
  }
  \item{\dots}{ other arguments, passed to the underlying \code{formula}
    method.
  }
}
  
\details{

  The first dimension is used as the variable on the vertical axis.  The
  last dimension is optionally used as a grouping variable (to produce
  stacked barcharts by default).  All other dimensions are used as
  conditioning variables.  The order of these variables cannot be
  altered (except by permuting the original argument using
  \code{\link{t}} or \code{\link{aperm}}).  For more flexibility, use
  the formula method after converting the table to a data frame using
  the relevant \code{\link[base:as.data.frame.table]{as.data.frame}}
  method.

}

\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}


\seealso{

  \code{\link{barchart}}, \code{\link{t}}, \code{\link{aperm}},
  \code{\link{table}}, \code{\link{panel.barchart}},
  \code{\link{Lattice}}

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
barchart(Titanic, scales = list(x = "free"),
         auto.key = list(title = "Survived"))
}
\keyword{hplot}
