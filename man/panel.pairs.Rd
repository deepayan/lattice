\name{panel.pairs}
\alias{panel.pairs}
\alias{diag.panel.splom}
\title{ Default Superpanel Function for splom}
\description{
  This is the default superpanel function for \code{splom}.
}
\synopsis{
panel.pairs(z,
             panel = "panel.splom",
             lower.panel = panel,
             upper.panel = panel,
             diag.panel = "diag.panel.splom",
             as.matrix = FALSE,

             groups = NULL,
             panel.subscripts,
             subscripts,
             pscales = 5,
             panel.number = 0,
             panel.counter = 0,

             prepanel.limits = function(x) if (is.factor(x)) levels(x) else
             extend.limits(range(as.numeric(x), na.rm = TRUE)),


             varname.col = add.text$col,
             varname.cex = add.text$cex,
             varname.font = add.text$font,
             varname.fontfamily = add.text$fontfamily,
             varname.fontface = add.text$fontface,

             axis.text.col = axis.text$col,
             axis.text.cex = axis.text$cex,
             axis.text.font = axis.text$font,
             axis.text.fontfamily = axis.text$fontfamily,
             axis.text.fontface = axis.text$fontface,

             axis.line.col = axis.line$col,
             axis.line.lty = axis.line$lty,
             axis.line.lwd = axis.line$lwd,

             ...)
diag.panel.splom(varname = NULL, limits, at = NULL, lab = NULL,
             draw = TRUE,

             varname.col = add.text$col,
             varname.cex = add.text$cex,
             varname.font = add.text$font,
             varname.fontfamily = add.text$fontfamily,
             varname.fontface = add.text$fontface,

             axis.text.col = axis.text$col,
             axis.text.alpha = axis.text$alpha,
             axis.text.cex = axis.text$cex,
             axis.text.font = axis.text$font,
             axis.text.fontfamily = axis.text$fontfamily,
             axis.text.fontface = axis.text$fontface,

             axis.line.col = axis.line$col,
             axis.line.alpha = axis.line$alpha,
             axis.line.lty = axis.line$lty,
             axis.line.lwd = axis.line$lwd,
             ...)
}
\usage{
panel.pairs(z,
            panel = "panel.splom",
            lower.panel = panel,
            upper.panel = panel,
            diag.panel = "diag.panel.splom",
            as.matrix = FALSE,
            groups = NULL,
            panel.subscripts,
            subscripts,
            pscales = 5,
            prepanel.limits = function(x) if (is.factor(x)) levels(x) else
            extend.limits(range(as.numeric(x), na.rm = TRUE)),
            ...)
diag.panel.splom(varname, limits, at, lab, draw = TRUE, ...)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{panel, lower.panel, upper.panel}{
    The panel function used to display each pair of variables. If
    specified, \code{lower.panel} and \code{upper.panel} are used for
    panels below and above the diagonal respectively.
  }
  \item{diag.panel}{
    The panel function used for the diagonals. See arguments to
    \code{diag.panel.splom} to know what arguments this function is
    passed when called.
  }
  \item{as.matrix}{
    logical. If \code{TRUE}, the layout of the panels will have origin
    on the top left instead of bottom left (similar to
    \code{pairs}). This is in essence the same functionality as provided
    by \code{as.table} for the panel layout
  }
  \item{groups}{ Grouping variable, if any }
  \item{panel.subscripts}{
    logical specifying whether the panel function accepts an argument
    named \code{subscripts}.
  }
  \item{subscripts}{ The indices of the rows of \code{z} that are to be
    displayed in this (super)panel.
  }
  \item{pscales}{ Controls axis labels, passed down from
    \code{splom}. If pscales is a single number, it indicates the
    approximate number of equally-spaced ticks that should appear on
    each axis. If pscales is a list, it should have p components, each
    of which is itself a list with the following valid components:

    at: a numeric vector specifying tick locations

    labels: character vector labels to go with at

    limits: numeric 2-vector specifying axis limits (should be made more
    flexible at some point to handle factors)

    These are specifications on a per-variable basis, and used on all
    four sides in  the diagonal cells used for labelling.

    Factor variables are labelled with the factor names. Use pscales=0
    to supress the axes entirely.
  }
  \item{prepanel.limits}{ The `regular' high level lattice plots such as
    \code{xyplot} use the \code{prepanel} function for deciding on axis
    limits from data. This function serves a similar function, and works
    on a per-variable basis, by calculating the limits, which can be
    overridden by the corresponding \code{limits} component in the
    \code{pscales} list.
  }
  \item{varname}{ (scalar) character string or expression that is to be
    written centred within the panel
  }
  \item{limits}{
    numeric of length 2, or, vector of characters, specifying the scale
    for that panel (used to calculate tick locations when missing)
  }
  \item{at}{ locations of tick marks}
  \item{lab}{ optional labels for tick marks}
  \item{draw}{
    logical, specifying whether to draw the tick marks and labels. If
    \code{FALSE}, only variable names are written
  }
  \item{\dots}{
    extra arguments, typically graphical parameters to control
    appearance of axes and variable names in the diagonal panels. The
    actual list of possible arguments is rather large, see the formal
    arguments to \code{panel.pairs} and \code{panel.diag.splom} (using
    \code{formals}) for all the possibilities.
  }
}
\details{
  This is the function that is actually passed in as the \code{panel}
  function in a trellis object produced by splom (taking the `panel'
  function as its argument).
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{splom}}}
\keyword{dplot}
















