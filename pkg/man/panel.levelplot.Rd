\name{panel.levelplot}
\alias{panel.levelplot}
\alias{panel.contourplot}
\title{ Default Panel Function for levelplot }
\description{
  This is the default panel function for \code{levelplot}.
}
\usage{
panel.levelplot(x, y, z, zcol,
                subscripts,
                at = mean(z),
                shrink,
                labels = NULL,
                label.style = c("mixed", "flat", "align"),
                contour = TRUE,
                region = TRUE,
                col = add.line$col,
                lty = add.line$lty,
                lwd = add.line$lwd,
                cex = add.text$cex,
                font = add.text$font,
                fontfamily = add.text$fontfamily,
                fontface = add.text$fontface,
                col.text = add.text$col,
                \dots,
                col.regions)
panel.contourplot(\dots)
}
\arguments{
  \item{x, y, z}{ variables defining the plot }
  \item{zcol}{
    color numbers for each x,y combination, used as indices for the
    color vector \code{col.regions}
  }
  \item{subscripts}{ which points of \code{x,y,z} to draw }
  \item{at}{ numeric vector specifying cut points }
  \item{shrink}{
    either a numeric vector of length 2 (meant to work as both x and y
    components), or a list with components x and y which are numeric
    vectors of length 2.  This allows the rectangles to be scaled
    proportional to the z-value. The specification can be made
    separately for widths (x) and heights (y).  The elements of the
    length 2 numeric vector gives the minimum and maximum proportion of
    shrinkage (corresponding to min and max of z).
  }
  \item{labels}{contour labels}
  \item{label.style}{
    controls choice of how label positions are determined.
  }
  \item{contour}{
    logical, specifying whether contour lines should be
    drawn
  }
  \item{region}{
    logical, specifying whether inter-contour regions should be filled
    with the appropriate color
  }
  \item{col, lty, lwd}{ graphical parameters for contour lines}
  \item{cex, col.text, font, fontfamily, fontface}{ graphical parameters for contour labels}
  \item{\dots}{ extra parameters }
  \item{col.regions}{ colors indexed by zcol, used if \code{region=TRUE}
  }
}

\details{

  The same function is used for both \code{levelplot} and
  \code{contourplot} (which differ only in default values of some
  arguments). \code{panel.contourplot} is a simple wrapper to
  \code{panel.levelplot}.

  When \code{contour=TRUE}, the \code{contourLines} function is used to
  calculate the contour lines.
} 

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{levelplot}}, \code{\link{contourLines}} }
\keyword{dplot}
















