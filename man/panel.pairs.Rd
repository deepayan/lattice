\name{F_1_panel.pairs}
\alias{panel.pairs}
\alias{diag.panel.splom}
\title{ Default Superpanel Function for splom}
\description{
  This is the default superpanel function for \code{splom}.
}
\usage{

panel.pairs(z,
            panel = lattice.getOption("panel.splom"),
            lower.panel = panel,
            upper.panel = panel,
            diag.panel = "diag.panel.splom",
            as.matrix = FALSE,
            groups = NULL,
            panel.subscripts,
            subscripts,
            pscales = 5,
%             packet.number, panel.number,
            prepanel.limits = function(x) if (is.factor(x)) levels(x) else
            extend.limits(range(as.numeric(x), finite = TRUE)),

            varname.col, varname.cex, varname.font,
            varname.fontfamily, varname.fontface,
            axis.text.col, axis.text.cex, axis.text.font,
            axis.text.fontfamily, axis.text.fontface,
            axis.line.col, axis.line.lty, axis.line.lwd,
            axis.line.alpha, axis.line.tck,
            \dots)
diag.panel.splom(x = NULL,
                 varname = NULL, limits, at = NULL, lab = NULL,
                 draw = TRUE,
                 varname.col, varname.cex,
                 varname.lineheight, varname.font,
                 varname.fontfamily, varname.fontface,
                 axis.text.col, axis.text.alpha,
                 axis.text.cex, axis.text.font, 
                 axis.text.fontfamily, axis.text.fontface,
                 axis.line.col, axis.line.alpha,
                 axis.line.lty, axis.line.lwd,
                 axis.line.tck,
                 \dots)
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
  \item{pscales}{ Controls axis labels, passed down from \code{splom}.
    If \code{pscales} is a single number, it indicates the approximate
    number of equally-spaced ticks that should appear on each axis.  If
    \code{pscales} is a list, it should have one component for each
    column in \code{z}, each of which itself a list with the following
    valid components:

    \code{at}: a numeric vector specifying tick locations

    \code{labels}: character vector labels to go with at

    \code{limits}: numeric 2-vector specifying axis limits (should be
    made more flexible at some point to handle factors)

    These are specifications on a per-variable basis, and used on all
    four sides in the diagonal cells used for labelling.  Factor
    variables are labelled with the factor names.  Use \code{pscales=0}
    to supress the axes entirely.
  }
  \item{prepanel.limits}{ The \sQuote{regular} high level lattice plots
    such as \code{xyplot} use the \code{prepanel} function for deciding
    on axis limits from data.  This function serves a similar function,
    and works on a per-variable basis, by calculating the limits, which
    can be overridden by the corresponding \code{limits} component in
    the \code{pscales} list.
  }
  \item{x}{
    data vector corresponding to that row / column (which will be the
    same for diagonal \sQuote{panels}).
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
%   \item{panel.number, packet.number}{ integer indices for panel and
%     packet number (see \code{\link{xyplot}}).  Passed on to
%     \code{panel}, \code{lower.panel}, \code{upper.panel} and
%     \code{diag.panel}. }
  \item{varname.col, varname.cex, varname.lineheight, varname.font,
    varname.fontfamily, varname.fontface}{ graphical parameters for the
    variable name in each diagonal panel }
  \item{axis.text.col, axis.text.cex, axis.text.font,
    axis.text.fontfamily, axis.text.fontface, axis.line.col,
    axis.line.lty, axis.line.lwd, axis.text.alpha, axis.line.alpha}{
    graphical parameters for axis tick marks and labels }
  \item{axis.line.tck}{ length of tick marks in diagonal panels }
  \item{\dots}{ extra arguments passed on to \code{panel},
    \code{lower.panel}, \code{upper.panel} and \code{diag.panel} from
    \code{panel.pairs}.  Currently ignored by \code{diag.panel.splom}. }

}

\details{
  \code{panel.pairs} is the function that is actually passed in as the
  \code{panel} function in a trellis object produced by splom (taking
  the \code{panel} function as its argument).

  Note that the axis labeling does not support date-time classes at
  present.

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{splom}}}
\keyword{dplot}

