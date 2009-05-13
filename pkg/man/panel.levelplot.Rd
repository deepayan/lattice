\name{panel.levelplot}
\alias{panel.levelplot}
\alias{panel.contourplot}
\title{ Default Panel Function for levelplot }
\description{
  This is the default panel function for \code{levelplot}.
}
\usage{
panel.levelplot(x, y, z, 
                subscripts,
                at = pretty(z),
                shrink,
                labels,
                label.style = c("mixed", "flat", "align"),
                contour = FALSE,
                region = TRUE,
                col = add.line$col,
                lty = add.line$lty,
                lwd = add.line$lwd,
                \dots,
                col.regions = regions$col,
                alpha.regions = regions$alpha)
panel.contourplot(\dots)
}
\arguments{
  \item{x, y, z}{ Variables defining the plot. }
  \item{subscripts}{
    Integer vector indicating what subset of \code{x}, \code{y} and
    \code{z} to draw.
  }
  \item{at}{
    Numeric vector giving breakpoints along the range of
    \code{z}.  See \code{\link{levelplot}} for details.
  }
  \item{shrink}{
    Either a numeric vector of length 2 (meant to work as both x and y
    components), or a list with components x and y which are numeric
    vectors of length 2.  This allows the rectangles to be scaled
    proportional to the z-value. The specification can be made
    separately for widths (x) and heights (y).  The elements of the
    length 2 numeric vector gives the minimum and maximum proportion of
    shrinkage (corresponding to min and max of z).
  }
  \item{labels}{
    Either a logical scalar indicating whether the labels are to be
    drawn, or a character or expression vector giving the labels
    associated with the \code{at} values.  Alternatively, \code{labels}
    can be a list with the following components:
    
    \describe{
    
      \item{\code{labels}:}{ a character or expression vector giving the
	labels.  This can be omitted, in which case the defaults will be
	used. }

      \item{\code{col, cex, alpha}:}{ graphical parameters for label
	texts }

      \item{\code{fontfamily, fontface, font}:}{ font used for the
	labels }
    }
    
  }
  \item{label.style}{
    Controls how label positions and rotation are determined.  A value
    of \code{"flat"} causes the label to be positioned where the contour
    is flattest, and the label is not rotated.  A value of
    \code{"align"} causes the label to be drawn as far from the
    boundaries as possible, and the label is rotated to align with the
    contour at that point.  The default is to mix these approaches,
    preferring the flattest location unless it is too close to the
    boundaries.
  }
  \item{contour}{
    A logical flag, specifying whether contour lines should be drawn.
  }
  \item{region}{
    A logical flag, specifying whether inter-contour regions should be
    filled with the appropriate color.
  }
  \item{col, lty, lwd}{ graphical parameters for contour lines}
%   \item{cex, col.text, font, fontfamily, fontface}{ graphical parameters for contour labels}
  \item{\dots}{ Extra parameters. }
  \item{col.regions}{
    A vector of colors, or a function to produce a vecor of colors, to
    be used if \code{region=TRUE}.  Each interval defined by \code{at}
    is assigned a color, so the number of colors actually used is one
    less than the length of \code{at}.  See \code{\link{level.colors}}
    for details on how the color assignment is done.
  }
  \item{alpha.regions}{
    numeric scalar controlling transparency of facets
  }
}

\details{
  The same panel function is used for both \code{levelplot} and
  \code{contourplot} (which differ only in default values of some
  arguments). \code{panel.contourplot} is a simple wrapper to
  \code{panel.levelplot}.

  When \code{contour=TRUE}, the \code{contourLines} function is used to
  calculate the contour lines.
} 

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{levelplot}},
  \code{\link{level.colors}},
  \code{\link{contourLines}}
}
\keyword{dplot}
















