\name{panel.barchart}
\alias{panel.barchart}
\title{ Default Panel Function for barchart}
\description{
  Default panel function for \code{barchart}.
}
\usage{
panel.barchart(x, y, box.ratio = 1,
               horizontal = TRUE,
               origin = NULL, reference = TRUE,
               stack = FALSE,
               groups = NULL, 
               col = if (is.null(groups)) bar.fill$col else superpose.fill$col,
               border = if (is.null(groups)) bar.fill$border else superpose.fill$border,
               lty = if (is.null(groups)) bar.fill$lty else superpose.fill$lty, 
               lwd = if (is.null(groups)) bar.fill$lwd else superpose.fill$lwd,
               \dots)
}
\arguments{
  \item{x}{ Extent of Bars. By default, bars start at left of panel,
    unless \code{origin} is specified, in which case they start there
  }
  \item{y}{ Horizontal location of bars, possibly factor}
  \item{box.ratio}{ratio of bar width to inter-bar space}
  \item{horizontal}{
    logical. If FALSE, the plot is \sQuote{transposed} in the sense that
    the behaviours of x and y are switched. x is now the
    \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
    explanation.
  }
  \item{origin}{ origin of the bars. For grouped displays with
    \code{stack = TRUE}, this argument is ignored and the origin set to
    0. Otherwise, defaults to \code{NULL}, in which case bars start at
    leftmost end of panel.
  }
  \item{reference}{ logical, whether a reference line is to be drawn at
    the origin
  }
  \item{stack}{ logical, relevant when groups is non-null. If
    \code{FALSE} (the default), bars for different values of the
    grouping variable are drawn side by side, otherwise they are
    stacked.
  }
  \item{groups}{ optional grouping variable}
  \item{col, border, lty, lwd} {
    Graphical parameters for the bars. By default, the trellis parameter
    \code{bar.fill} is used if there is no grouping variable, otherwise
    \code{superpose.fill} is used.
  }
  \item{\dots}{ extra arguments will be accepted but ignored }
}

\details{
  A barchart is drawn in the panel. Note that most arguments controlling
  the display can be supplied to the high-level \code{barchart} call
  directly.
}

\seealso{
  \code{\link{barchart}}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

