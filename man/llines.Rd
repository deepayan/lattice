\name{llines}
\alias{larrows}
\alias{llines}
\alias{lplot.xy}
\alias{lpoints}
\alias{lsegments}
\alias{ltext}
\alias{lrect}
\alias{lpolygon}
\alias{panel.arrows}
\alias{panel.lines}
\alias{panel.points}
\alias{panel.segments}
\alias{panel.text}
\alias{panel.rect}
\alias{panel.polygon}
\title{Replacements of traditional graphics functions}

\description{

  These functions are intended to replace common low level traditional
  graphics functions, primarily for use in panel functions.  The
  originals can not be used (at least not easily) because lattice panel
  functions need to use grid graphics.  Low level drawing functions in
  grid can be used directly as well, and is often more flexible.  These
  functions are provided for convenience and portability.

}

\usage{
larrows(x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL,
        angle = 30, code = 2, length = 0.25, unit = "inches",
        col, alpha, lty, lwd, fill = NULL, \dots)
llines(x, y = NULL, type = "l",
       col, alpha, lty, lwd, \dots)
lplot.xy(xy, type, pch, lty, col, cex, lwd,
         font, fontfamily, fontface,
         col.line, alpha, fill,
         origin = 0, \dots)
lpoints(x, y = NULL, type = "p", col, pch, alpha, fill,
        font, fontfamily, fontface, cex, \dots)  
lsegments(x0, y0, x1, y1, x2, y2,
          col, alpha, lty, lwd, \dots)
ltext(x, y = NULL, labels = seq(along = x),
      col, alpha, cex, srt = 0,
      lineheight, font, fontfamily, fontface,
      adj = c(0.5, 0.5), pos = NULL, offset = 0.5, \dots)  
lrect(xleft, ybottom, xright, ytop,
      x = (xleft + xright) / 2,
      y = (ybottom + ytop) / 2,
      width = xright - xleft,
      height = ytop - ybottom,
      col = "transparent",
      border = "black",
      lty = 1, lwd = 1, alpha = 1,
      just = "center",
      hjust = NULL, vjust = NULL,
      \dots)
lpolygon(x, y = NULL,
         border = "black", col = "transparent", \dots)
panel.arrows(\dots)
panel.lines(\dots)
panel.points(\dots)
panel.segments(\dots)
panel.text(\dots)
panel.rect(\dots)
panel.polygon(\dots)
}
\arguments{

  \item{ x, y, x0, y0, x1, y1, x2, y2, xy}{ locations. \code{x2} and
    \code{y2} are available for for S compatibility.  }
  \item{length, unit}{
    determines extent of arrow head.  \code{length} specifies the length
    in terms of \code{unit}, which can be any valid grid unit as long as
    it doesn't need a \code{data} argument.  \code{unit} defaults to
    inches, which is the only option in the base version of the
    function, \code{\link{arrows}}.  }
  \item{angle, code, type, labels, srt, adj, pos, offset}{ arguments
    controlling behaviour.  See respective base functions for details. }
  \item{col, alpha, lty, lwd, fill, pch, cex, lineheight, font,
    fontfamily, fontface, col.line, border}{ graphical parameters.
    \code{fill} applies to points when \code{pch} is in \code{21:25} and
    specifies the fill color, similar to the \code{bg} argument in the
    base graphics function \code{\link{points}}. For devices that
    support alpha-transparency, a numeric argument \code{alpha} between
    0 and 1 can controls transparency.  Be careful with this, since for
    devices that do not support alpha-transparency, nothing will be
    drawn at all if this is set to anything other than 0.
  }
  \item{origin}{ for \code{type="h"} or \code{type="H"}, the value to
    which lines drop down. }
  \item{xleft, ybottom, xright, ytop}{ see \code{\link{rect}}}
  \item{width, height, just, hjust, vjust}{ finer control over
    rectangles, see \code{\link[grid:grid.rect]{grid.rect}} }
  \item{\dots}{ extra arguments, passed on to lower level functions as
    appropriate.  }
}


\details{
  These functions are meant to be grid replacements of the corresponding
  base R graphics functions, to allow existing Trellis code to be used
  with minimal modification.  The functions \code{panel.*} are
  essentally identical to the \code{l*} versions, are recommended for
  use in new code (as opposed to ported code) as they have more readable
  names.

  See the documentation of the base functions for usage. Not all
  arguments are always supported. All these correspond to the default
  methods only.
%   For \code{ltext}, only values 0, .5 and 1 for \code{adj}
%   have any effect.
}
\note{
  There is a new \code{type="H"} option wherever appropriate, which is
  similar to \code{type="h"}, but with horizontal lines.
}

\seealso{\code{\link{points}}, \code{\link{lines}}, \code{\link{rect}},
  \code{\link{text}}, \code{\link{segments}}, \code{\link{arrows}},
  \code{\link{Lattice}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
