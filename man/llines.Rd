\name{llines}
\alias{larrows}
\alias{llines}
\alias{lplot.xy}
\alias{lpoints}
\alias{lsegments}
\alias{ltext}
\title{Lattice Replacements of base graphics functions}
\description{
  These functions are intended to replace some commonly used base R
  graphics functions in panel functions.
}
\synopsis{
larrows(x0, y0, x1, y1, x2, y2, angle = 30, code = 2, length = NULL, 
    proportion = 0.05, \dots)
llines(x, y = NULL, type = "l", col = plot.line$col, lty = plot.line$lty, 
    lwd = plot.line$lwd, \dots)
lplot.xy(xy, type, pch = 1, lty = 1, col = 1, cex = 1, lwd = 1,
         font = 1, fontfamily = NULL, fontface = NULL,
         col.line = col, \dots)  
lpoints(x, y = NULL, type = "p", col = plot.symbol$col,
        pch = plot.symbol$pch, 
        font = plot.symbol$font,
        fontfamily  = plot.symbol$fontfamily,
        fontface = plot.symbol$fontface,
        cex = plot.symbol$cex, \dots)  
lsegments(x0, y0, x1, y1, x2, y2, col = add.line$col, lty = add.line$lty, 
    lwd = add.line$lwd, \dots)
ltext(x, y = NULL, labels = seq(along = x), col = add.text$col,
    cex = add.text$cex, srt = 0,
    font, fontfamily = add.text$fontfamily,
    fontface = add.text$fontface, adj = c(0.5, 0.5), pos = NULL,
    offset = 0.5, \dots)  
}
\usage{
larrows(x0, y0, x1, y1, proportion, \dots)
llines(x, y, \dots)
lplot.xy(xy, \dots)
lpoints(x, \dots)
lsegments(x0, y0, x1, y1, \dots)
lsegments(x1, y1, x2, y2, \dots)
ltext(x, \dots)
}
\arguments{
  \item{ x, y, x0, y0, x1, y1, x2, y2, xy}{ locations. x2, y2 present
    for S compatibility
  }
  \item{proportion}{ The proportion of the arrow edges compared to
  the whole line. This is different from \code{arrows}}
  \item{\dots}{ other arguments}
}
\details{
  These functions are meant to be grid replacements of the corresponding
  base R graphics functions, to allow existing Trellis code to be used
  with minimal modification. 

  See the documentation of the base functions for usage. Not all
  arguments are always supported. All these correspond to the default
  methods only. For \code{ltext}, only values 0, .5 and 1 for \code{adj}
  have any effect.
}
\seealso{\code{\link{points}}, \code{\link{lines}},
  \code{\link{text}}, \code{\link{segments}},
  \code{\link{Lattice}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
