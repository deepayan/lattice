\name{panel.superpose}
\alias{panel.superpose}
\alias{panel.superpose.2}
\title{ Panel Function for Display Marked by groups }
\description{
  This is an useful panel function for trellis displays. Uses a grouping
  variable, plotting the \code{x} (and \code{y} where appropriate)
  variables with different graphical properties for each distinct value
  of the grouping variable. 
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
             ...)
panel.superpose.2(x, y, subscripts, groups, col, col.line = superpose.line$col,
                  col.symbol = superpose.symbol$col, pch = superpose.symbol$pch,
                  cex = superpose.symbol$cex, lty = superpose.line$lty,
                  lwd = superpose.line$lwd, type="p", ...)
}
\usage{
panel.superpose(x, y, panel.groups = "panel.xyplot", subscripts, groups, type="p", ...)
panel.superpose.2(x, y, subscripts, groups, type="p", ...)
}
\arguments{
  \item{x,y}{ coordinates of the points to be displayed}
  \item{panel.groups}{ the panel function to be used for each group of
    points. Defaults to \code{panel.xyplot} (behaviour in S)}
  \item{subscripts}{ subscripts giving indices in original data frame}
  \item{groups}{ a grouping variable. Different graphical parameters
    will be used to plot the subsets of observations given by each
    distinct value of \code{groups}. The default graphical parameters
    are obtained from \code{superpose.symbol} and \code{superpose.line}
    using \code{\link{trellis.par.get}} wherever appropriate
  }
  \item{type}{ character vector specifying what should be drawn for each
    group. See \code{\link{panel.xyplot}} for details of what the values
    can be. This is meaningful for \code{panel.superpose} only when
    \code{panel.groups} is \code{panel.xyplot}.

    The 2 functions \code{panel.superpose} (with \code{panel.xyplot})
    and \code{panel.superpose.2} differ only in the way the \code{type}
    argument is interpreted. For the former, the interpretation is the
    same as for \code{panel.xyplot} for each of the unique groups. For
    the latter, the \code{type} vector is replicated to be as long as
    the number of unique values in \code{groups}, and the points
    corresponding to the different groups are plotted according to the
    corresponding type. 

    See \code{xyplot} documentation for an example.
  }
  \item{\dots}{ other arguments (mostly graphical parameters, replicated
    to be as long as the number of groups), ultimately passed down to
    \code{panel.xyplot}. In particular, colour can be specified using 3
    arguments (\code{col, col.line, col.symbol}), as in
    \code{panel.xyplot}.
  }
}
\seealso{
  Different functions when used as \code{panel.groups} gives different
  types of plots, for example \code{\link{panel.xyplot}},
  \code{\link{panel.dotplot}} and \code{\link{panel.linejoin}} (This can
  be used to produce interaction plots).
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}
  (\code{panel.superpose.2} contributed by Neil Klepeis)
}
\keyword{dplot}
