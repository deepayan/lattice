\name{panel.axis}
\alias{panel.axis}
\title{Panel Function for Drawing Axis Ticks and Labels}
\description{
  This is the function used by lattice to draw axes. It's typically not
  used by users, except those wishing to create advanced
  annotation. Keep in mind issues of clipping when trying to use it
  as part of the panel function.
}
\synopsis{
panel.axis(side = c("bottom", "left", "top", "right"),
             at = pretty(scale.range),
             labels = TRUE,
             draw.labels = TRUE,
             check.overlap = FALSE,
             outside = FALSE,
             tick = TRUE,
             half = !outside,
             which.half = switch(side, bottom = "lower", left = "upper", top = "upper", right = "lower"),

             tck = as.numeric(tick),
             rot = if (is.logical(labels)) 0 else c(90, 0),

             text.col = axis.text$col,
             text.alpha = axis.text$alpha,
             text.cex = axis.text$cex,
             text.font = axis.text$font,
             text.fontfamily = axis.text$fontfamily,
             text.fontface = axis.text$fontface,

             line.col = axis.line$col,
             line.lty = axis.line$lty,
             line.lwd = axis.line$lwd,
             line.alpha = axis.line$alpha)
}
\usage{
panel.axis(side = c("bottom", "left", "top", "right"),
           at,
           labels = TRUE,
           draw.labels = TRUE,
           check.overlap = FALSE,
           outside = FALSE,
           tick = TRUE,
           half = !outside,
           which.half,
           tck = 1,
           rot, \dots)
}
\arguments{
  \item{side}{
    character string indicating which side axes are to be drawn
    on. Partial specification is allowed.
  }
  \item{at}{ location of labels }
  \item{labels}{
    the labels to go along with \code{at}. The labels can be a character
    vector or a vector of expressions. Alternatively, this can be a
    logical. If TRUE, the labels are derived from \code{at}, otherwise,
    labels are empty.
  }
  \item{draw.labels}{
    logical indicating whether labels are to be drawn
  }
  \item{check.overlap}{
    logical, whether to check for overlapping of labels.  This also has
    the effect of removing \code{at} values that are `too close' to the
    limits.
  }
  \item{outside}{
    logical, whether to the labels draw outside the panel or inside.
  }
  \item{tick}{ logical, whether to draw the tickmarks }
  \item{half}{
    logical, whether only half of scales will be drawn for each side
  }
  \item{which.half}{
    character string, one of ``lower'' and ``upper''. Indicates which
    half is to be used for tick locations if \code{half=TRUE}.  Defaults
    to whatever is suitable for \code{splom}
  }
  \item{tck}{
    numeric scalar, multiplier for tick length. Can be negative.
  }
  \item{rot}{
    rotation angles for labels in degrees. Can be a vector of length 2
    for x- and y-axes respectively
  }
  \item{\dots}{
    certain graphical parameters (fonts, color, etc) can be
    supplied. See the formal argument list for valid names.
  }
}
\details{
  \code{panel.axis} can draw axis tick marks inside or outside a
  panel (more precisely, a grid viewport). It honours the (native) axis
  scales. Used in \code{\link{panel.pairs}} for \code{\link{splom}}, as
  well as for all the usual axis drawing by the print method for
  ``trellis'' objects. It can also be used to enhance plots `after the
  fact' by adding axes.
}

\seealso{
  \code{\link{Lattice}}, \code{\link{xyplot}},
  \code{\link{trellis.focus}}
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




