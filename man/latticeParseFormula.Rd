\name{G latticeParseFormula}
\alias{latticeParseFormula}
\title{Parse Trellis formula}
\usage{
latticeParseFormula(model, data, dimension = 2,
                    subset = TRUE, groups = NULL,
                    multiple, outer,
                    subscripts, drop, case.names)
}
\description{
  this function is used by high level Lattice functions such as
  \code{\link{xyplot}} to parse the formula argument and evaluate various
  components of the data.
}

\arguments{
  \item{model}{
    the model/formula to be parsed.  This can be in either of two
    possible forms, one for 2d and one for 3d formulas, determined by
    the \code{dimension} argument. The 2d formulas are of the form
    \code{y ~ x| g1 * ... *gn}, and the 3d formulas are of the form
    \code{z ~ x * y | g1 * ... * gn}. In the first form, \code{y} may
    be omitted. The conditioning variables \code{g1, ..., gn} can be
    omitted in either case.
  }
  \item{data}{ the environment/dataset where the variables in the 
    formula are evaluated.
  }
  \item{dimension}{ dimension of the model, see above. }
  \item{subset}{ index for choosing a subset of the data frame. }
  \item{groups}{ the grouping variable, if present. }
  \item{multiple, outer}{
    logicals, determining how a \sQuote{+} in the y and x components of
    the formula are processed. See \code{\link{xyplot}} for details.
  }
  \item{subscripts}{ logical, whether subscripts are to be calculated. }
  \item{drop}{
    logical or list, similar to the \code{drop.unused.levels} argument
    in \code{\link{xyplot}}, indicating whether unused levels of
    conditioning factors and data variables that are factors are to be
    dropped.
  }
  \item{case.names}{
    either a logical, or a character string giving case names. If
    \code{TRUE}, the row names of \code{data} via
    \code{\link{case.names}} is used.
  }
}

\value{

  Returns a list with several components, including \code{left, right,
    left.name, right.name, condition} for 2-D, and \code{left, right.x,
    right.y, left.name, right.x.name, right.y.name, condition} for
  3-D. Other possible components are \code{groups}, \code{subscr}, and
  \code{case.names}.

}

\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}}
}
\author{Saikat DebRoy, Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{utilities}
