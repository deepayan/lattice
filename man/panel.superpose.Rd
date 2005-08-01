\name{panel.superpose}
\alias{panel.superpose}
\alias{panel.superpose.2}
\title{ Panel Function for Display Marked by groups }
\description{
  These are panel functions for Trellis displays useful when a grouping
  variable is specified for use within panels. The \code{x} (and
  \code{y} where appropriate) variables are plotted with different
  graphical parameters for each distinct value of the grouping variable.
}

\synopsis{
panel.superpose(x, y = NULL, subscripts, groups,
             panel.groups = "panel.xyplot",
             col,
             col.line = superpose.line$col,
             col.symbol = superpose.symbol$col,
             pch = superpose.symbol$pch,
             cex = superpose.symbol$cex, 
             font = superpose.symbol$font,
             fontface = superpose.symbol$fontface,
             fontfamily = superpose.symbol$fontfamily,
             lty = superpose.line$lty,
             lwd = superpose.line$lwd,
             alpha = superpose.symbol$alpha,
             type = "p",
             ...)
panel.superpose.2(x, y = NULL, subscripts, groups,
                  panel.groups = "panel.xyplot",
                  col,
                  col.line = superpose.line$col,
                  col.symbol = superpose.symbol$col,
                  pch = superpose.symbol$pch,
                  cex = superpose.symbol$cex, 
                  font = superpose.symbol$font, 
                  fontface = superpose.symbol$fontface, 
                  fontfamily = superpose.symbol$fontfamily, 
                  lty = superpose.line$lty,
                  lwd = superpose.line$lwd,
                  alpha = superpose.symbol$alpha,
                  type = "p",
                  ...)
}
\usage{
panel.superpose(x, y, panel.groups = "panel.xyplot",
                subscripts, groups, type = "p", ...)
panel.superpose.2(x, y, panel.groups = "panel.xyplot",
                  subscripts, groups, type = "p", ...)
}
\arguments{
  \item{x,y}{ coordinates of the points to be displayed}
  \item{panel.groups}{
    the panel function to be used for each group of points. Defaults to
    \code{panel.xyplot} (behaviour in S)
  }
  \item{subscripts}{ subscripts giving indices in original data frame }
  \item{groups}{ a grouping variable. Different graphical parameters
    will be used to plot the subsets of observations given by each
    distinct value of \code{groups}. The default graphical parameters
    are obtained from \code{superpose.symbol} and \code{superpose.line}
    using \code{\link{trellis.par.get}} wherever appropriate
  }
  \item{type}{
    usually a character vector specifying what should be drawn for each
    group, passed on to the \code{panel.groups} function, which must
    know what to do with it.  By default, this is
    \code{\link{panel.xyplot}}, whose help page describes the admissible
    values.

    The two functions \code{panel.superpose} and
    \code{panel.superpose.2} differ only in the way the \code{type}
    argument is interpreted.  For the former, the interpretation is the
    same as for \code{panel.xyplot} for each of the unique groups.  In
    other words, if \code{type} is a vector, all the individual
    components are honoured concurrently.  For the latter, \code{type}
    is replicated to be as long as the number of unique values in
    \code{groups}, and one component used for the points corresponding
    to the each different group.  Even in this case, it is possible to
    request multiple types per group, specifying \code{type} as a list,
    each component being the desired \code{type} vector for the
    corresponding plot.

    In \code{panel.superpose}, any occurrence of \code{"g"} in
    \code{type} causes a grid to be drawn, and all such occurrences are
    removed before \code{type} is passed on to \code{panel.groups}.
  }
  \item{\dots}{ other arguments (mostly graphical parameters, replicated
    to be as long as the number of groups), ultimately passed down to
    \code{panel.xyplot}. In particular, colour can be specified using 3
    arguments (\code{col}, \code{col.line} and \code{col.symbol}), as
    described in \code{\link{panel.xyplot}}.
  }
}
\seealso{
  Different functions when used as \code{panel.groups} gives different
  types of plots, for example \code{\link{panel.xyplot}},
  \code{\link{panel.dotplot}} and \code{\link{panel.linejoin}} (This can
  be used to produce interaction plots).
}

\author{
  Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}
  (\code{panel.superpose.2} originally contributed by Neil Klepeis)
}
\keyword{dplot}
