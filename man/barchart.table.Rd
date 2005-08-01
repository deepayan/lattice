\name{barchart.table}
\title{table methods for barchart and dotplot}
\alias{barchart.table}
\alias{barchart.array}
\alias{barchart.matrix}
\alias{dotplot.table}
\alias{dotplot.array}
\alias{dotplot.matrix}
\synopsis{
\method{barchart}{table}(formula, data = NULL, groups = TRUE,
             origin = 0, stack = TRUE, ...)
\method{dotplot}{table}(formula, data = NULL, groups = TRUE, ...)
}
\usage{
\method{barchart}{table}(formula, groups = TRUE,
         origin = 0, stack = TRUE, \dots)

\method{barchart}{array}(formula, \dots)

\method{barchart}{matrix}(formula, \dots)

\method{dotplot}{table}(formula, groups = TRUE, \dots)

\method{dotplot}{array}(formula, \dots)

\method{dotplot}{matrix}(formula, \dots)
}

\description{
  Contingency tables are often displayed using barcharts and dotplots.
  These methods are provided for convenience and operate directly on
  tables.  Arrays and matrices are simply coerced to be a table.
}



\arguments{
  \item{formula}{
    a \code{table}, \code{array} or \code{matrix} object.
  }
  \item{groups}{
    logical, whether to use the last dimension as the grouping variable
    in the display.
  }
  \item{origin, stack}{
    arguments to \code{panel.barchart} controlling the display.  The
    defaults for the \code{table} method are diffrent.
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
  altered (except by permuting the original argument usinf \code{t} or
  \code{aperm}).  For more flexibility, use the formula method after
  converting the table to a data frame using \code{as.data.frame}.

}


\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
}


\seealso{

  \code{\link{barchart}}, \code{\link{t}}, \code{\link{aperm}},
  \code{\link{table}}, \code{\link{panel.barchart}},
  \code{\link{Lattice}}

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{

barchart(Titanic, scales = list(x = "free"))

}
\keyword{hplot}
