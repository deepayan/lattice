\name{panel.bwplot}
\alias{panel.bwplot}
\title{ Default Panel Function for bwplot }
\description{
  This is the default panel function for \code{bwplot}.
}
\synopsis{
panel.bwplot(x, y, box.ratio=1, horizontal = TRUE, pch=box.dot$pch,
             col=box.dot$col, cex = box.dot$cex,
             font = box.dot$font, fontfamily = box.dot$fontfamily,
             fontface = box.dot$fontface,
             fill = box.rectangle$fill, varwidth = FALSE,
             levels.fos = NULL, coef = 1.5, ...)
}
\usage{
panel.bwplot(x, y, box.ratio = 1, horizontal, pch, col, cex,
            fill = "transparent", varwidth = FALSE, coef = 1.5, ...)
}
\arguments{
  \item{x, y}{ numeric vector or factor. Boxplots drawn for each unique value of
    \code{y} (\code{x}) if \code{horizontal} is \code{TRUE} (\code{FALSE})}
  \item{box.ratio}{ ratio of box height to inter box space}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{pch, col, cex}{ graphical parameters controlling the dot}
  \item{fill}{ color to fill the boxplot}
  \item{varwidth}{ logical. If TRUE, widths of boxplots are proportional
    to the number of points used in creating it.}
  \item{coef}{ passed to \code{boxplot.stats}}
  \item{\dots}{ graphical parameters can be supplied}
}
\details{
  Creates Box and Whisker plot of \code{x} for every level of
  \code{y}. The actual boxplot statistics are calculated using
  \code{boxplot.stats}. Note that most arguments controlling the display
  can be supplied to the high-level \code{bwplot} call directly.
}

\seealso{
  \code{\link{bwplot}}, \code{\link{boxplot.stats}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















