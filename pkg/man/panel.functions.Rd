\name{panel.functions}
\alias{panel.functions}
\alias{panel.abline}
\alias{panel.curve}
\alias{panel.rug}
\alias{panel.fill}
\alias{panel.grid}
\alias{panel.lmline}
\alias{panel.loess}
\alias{panel.linejoin}
\alias{panel.axis}
\alias{panel.identify}
\alias{panel.mathdensity}
\title{Useful Panel Functions}
\description{
  These are predefined panel functions available in lattice for use in
  constructing new panel functions (usually on-the-fly).
}
\synopsis{
panel.abline(a, b = NULL, h = numeric(0), v = numeric(0),
    col,
    col.line = add.line$col,
    lty = add.line$lty,
    lwd = add.line$lwd, 
    ...)
panel.curve(expr, from, to, n = 101,
            curve.type = "l",
            col = add.line$col,
            lty = add.line$lty,
            lwd = add.line$lwd,
            type = NULL,
            ...)
panel.rug(x = NULL, y = NULL,
          regular = TRUE, 
          start = if (regular) 0 else 0.97,
          end = if (regular) 0.03 else 1,
          col, ...)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    ...)
panel.axis(side = 1:4,
             at,
             labels = TRUE,
             tick = TRUE,
             half = TRUE,
             which.half = c("lower", "upper", "upper","lower"),

             tck = as.numeric(tick),
             rot = if (is.logical(labels)) 0 else c(90, 0),

             text.col,
             text.cex,

             text.font,
             text.fontfamily,
             text.fontface,

             line.col,
             line.lty,
             line.lwd)
}
\usage{
panel.abline(a, b, ...)
panel.abline(coef, ...)
panel.abline(reg, ...)
panel.abline(h= ,v= , ...)
panel.curve(expr, from, to, n = 101,
            curve.type = "l", ...)
panel.rug(x = NULL, y = NULL,
          regular = TRUE, start, end, \dots)
panel.fill(col="grey", ...)
panel.grid(h=3, v=3, ...)
panel.lmline(x,y,...) = panel.abline(lm(y~x),...)
panel.loess(x, y, span = 2/3, degree = 1,
            family = c("symmetric", "gaussian"),
            evaluation = 50, \dots)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, ...)
panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1))
panel.axis(side = 1:4, at,
           labels = TRUE, tick = TRUE,
           half = TRUE, which.half = c("lower", "upper", "upper","lower"),
           tck = 1, rot)
panel.identify(x, y = NULL, labels = seq(along = x),
               n = length(x), offset = 0.5,
               threshold = 18, ...)
}
\arguments{
  \item{x, y}{ variables defining the contents of the panel}
  \item{a, b}{ Coefficients of the line to be added}
  \item{coef}{ Coefficients of the line to be added as a length 2 vector}
  \item{reg}{ A regression object. The corresponding fitted line will
    be drawn }
  \item{h,v}{ For \code{panel.abline}, numerical vectors giving y and x
    locations respectively of horizontal and vertical lines to be
    added to the plot. For panel.grid, number of horizontal and vertical
    reference lines to be added to the plot; h=-1 and v=-1 make the
    grids aligned with the axis labels (this doesn't always work).
  }
  \item{expr}{ expression as a function of x or a function to plot as a
    curve
  }
  \item{regular}{ logical indicating whether `rug' to be drawn on the
    regular side (left / bottom) or not (right / top)
  }
  \item{start, end}{ endpoints of rug segments, in normalized parent
    coordinates (between 0 and 1). Defaults depend on value of regular,
    and cover 3\% of the panel width and height
  }
  \item{from, to}{ optional lower and upper x-limits of curve. If
    missing, limits of current panel are used
  }
  \item{n}{ for \code{panel.curve}, the number of points to use for
    drawing the curve. For \code{panel.identify}, the number of points
    to identify by default (overridden by a right click)
  }


  \item{curve.type}{ type of curve ('p' for points, etc), passed to
    \code{llines}
  }
  \item{col}{color}
  \item{span, degree, family, evaluation}{ arguments to 
    \code{loess.smooth}, around which \code{panel.loess} is essentially
    a wrapper
  }
  \item{fun}{ the function that will be applied to the subset of x(y)
    determined by the unique values of y(x)}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{dmath}{ A vectorized function that produces density values
    given a numeric vector named \code{x}, e.g., \code{dnorm}}
  \item{args}{ list giving additional arguments to be passed to dmath}

  \item{side}{ subset of 1:4, indicating which side which }
  \item{at}{ location of labels. No default.}
  \item{labels}{ for \code{panel.axis}, logical indicating whether
    labels are to be drawn, or the labels to go along with \code{at}.
    For \code{panel.identify}, an optional vector of labels associated
    with each point.
  }
  \item{tick}{ logical, whether to draw ticks }
  \item{half}{
    logical, whether only half of scales will be drawn for each side
  }
  \item{which.half}{
    character vector of length 4 (recycled if necessary), each being
    either ``lower'' or ``upper''. Indicates which half is to be used
    for tick locations if \code{half = TRUE}, for sides 1 to 4
  }
  \item{tck}{ numeric scalar, tick length }
  \item{rot}{
    numeric 4-vector (recycled if necessary), rotation angles for sides
    1 to 4
  }
  \item{offset}{ the labels are printed either below, above, to the left
    or to the right of the identified point, depending on the relative
    location of the mouse click. The \code{offset} specifies (in "char"
    units) how far from the identified point the labels should be
    printed.
  }
  \item{threshold}{ threshold in grid's "points" units. Points further
    than these from the mouse click position are not considered}

  \item{\dots}{ graphical parameters can be supplied. see function
    definition for details. Color can usually be specified by \code{col,
      col.line} and \code{col.symbol}, the last two overriding the first
    for lines and points respectively.
  }
}
\details{
  \code{panel.abline} adds a line of the form \code{y=a+bx} or vertical
  and/or horizontal lines. Graphical parameters are obtained from 
  \code{reference.line} for panel.grid, and \code{add.line} for the
  others (can be set using \code{trellis.par.set} )

  \code{panel.curve} adds a curve, similar to what \code{curve} does
  with \code{add = TRUE}. Graphical parameters for the line are obtained
  from the \code{add.line} setting.

  \code{panel.linejoin} treats one of x and y as a factor (according to
  the value of \code{horizontal}, calculates \code{fun} applied to the
  subsets of the other variable determined by each unique value of the
  factor, and joins them by a line. Can be used in conjunction with
  \code{panel.xyplot} and more commonly with panel.superpose to produce
  interaction plots. See \code{\link{xyplot}} documentation for an example.

  \code{panel.mathdensity} plots a (usually theoretical) probability
  density function. Can be useful in conjunction with \code{histogram}
  and \code{densityplot} to visually estimate goodness of fit.

  \code{panel.axis} draws axis tick marks INSIDE a panel. It honours the
  (native) axis scales within the panel, but the locations need to be
  explicitly specified. Used in \code{\link{panel.pairs}} for
  \code{\link{splom}}.

  \code{panel.identify} is similar to \code{\link{identify}}. When
  called as part of the panel function, it waits for the user to
  identify points (in the panel being drawn) via mouse clicks.
  Clicks other than left-clicks terminate the procedure. It is less
  sophisticated than \code{identify} in the sense that it does not keep
  track of points already identified.

}

\seealso{\code{\link[stats:scatter.smooth]{loess.smooth}},
  \code{\link{identify}}, \code{\link{trellis.par.get}}
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




