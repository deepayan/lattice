\name{panel.violin}
\alias{panel.violin}
\title{ Panel Function to create Violin Plots }
\description{
  This is a panel function that can create a violin plot.  It is
  typically used in a high-level call to \code{bwplot}.
}
\usage{

panel.violin(x, y, box.ratio = 1, horizontal = TRUE,
             alpha, border, lty, lwd, col,
             varwidth = FALSE,
             bw, adjust, kernel, window, 
             width, n = 50, from, to, cut,
             na.rm, \dots)
}
\arguments{
  \item{x, y}{
    numeric vector or factor. Violin plots are drawn for each unique
    value of \code{y} (\code{x}) if \code{horizontal} is \code{TRUE}
    (\code{FALSE})
  }
  \item{box.ratio}{
    ratio of height of a violin and inter violin space
  }
  \item{horizontal}{
    logical. If FALSE, the plot is \sQuote{transposed} in the sense that
    the behaviours of \code{x} and \code{y} are switched.  \code{x} is
    now the \sQuote{factor}. See documentation of \code{\link{bwplot}}
    for a fuller explanation.
  }
  \item{alpha, border, lty, lwd, col}{
    graphical parameters controlling the violin.  Defaults are taken
    from the \code{"plot.polygon"} settings.
  }
  \item{varwidth}{
    logical. If \code{FALSE}, the densities are scaled separately for
    each group, so that the maximum value of the density reaches the
    limit of the allocated space for each violin (as determined by
    \code{box.ratio}).  If \code{TRUE}, densities across violins will
    have comparable scale.
  }
  \item{bw, adjust, kernel, window, width, n, from, to, cut, na.rm}{
    arguments to \code{\link{density}}, passed on as appropriate }
  \item{\dots}{
    arguments passed on to \code{density}.
  }
}

\details{
  Creates Violin plot of \code{x} for every level of \code{y}. Note that
  most arguments controlling the display can be supplied to the
  high-level (typically \code{bwplot}) call directly.
}

\examples{

bwplot(voice.part ~ height, singer,
       panel = function(..., box.ratio) {
           panel.violin(..., col = "transparent",
                        varwidth = FALSE, box.ratio = box.ratio)
           panel.bwplot(..., fill = NULL, box.ratio = .1)
       } )

}

\seealso{
  \code{\link{bwplot}}, \code{\link{density}}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

