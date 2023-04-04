\name{F_1_panel.dotplot}
\alias{panel.dotplot}
\title{ Default Panel Function for dotplot}
\description{
  Default panel function for \code{dotplot}.
}
\usage{
panel.dotplot(x, y, horizontal = TRUE,
              pch, col, lty, lwd, col.line,
              levels.fos,
              groups = NULL,
              \dots,
              grid = lattice.getOption("default.args")$grid,
              identifier = "dotplot")
}
\arguments{
  \item{x,y}{ variables to be plotted in the panel. Typically y is the
    \sQuote{factor} }
  \item{horizontal}{ logical. If FALSE, the plot is \sQuote{transposed}
    in the sense that the behaviours of x and y are switched. x is now
    the \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
    explanation.
  }
  \item{pch, col, lty, lwd, col.line}{ graphical parameters}
  \item{levels.fos}{ locations where reference lines will be drawn }
  \item{groups}{ grouping variable (affects graphical parameters)}
  \item{\dots}{
    extra parameters, passed to \code{panel.xyplot} which is responsible
    for drawing the foreground points (\code{panel.dotplot} only draws
    the background reference lines).
  }
  \item{grid}{
    A logical flag, or list specifying whether and how a background grid
    should be drawn.  In its general form \code{grid} can be a list of
    arguments to be supplied to \code{\link{panel.grid}}, which is
    called with those arguments. If \code{FALSE}, no grid lines are
    drawn. \code{grid = TRUE} is roughly equivalent to \code{list(h = 0,
    v = -1)} if \code{horizontal = TRUE} and \code{list(h = -1, v = 0)}
    if \code{horizontal = FALSE}. In other words, grid lines are drawn
    only for the numeric axis, as reference lines for the categorical
    axis are drawn regardless of the value of \code{grid}.
  }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}
\details{
  Creates (possibly grouped) Dotplot of \code{x} against \code{y} or
  vice versa
}
\seealso{\code{\link{dotplot}}}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
















