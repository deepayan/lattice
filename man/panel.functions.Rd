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
          x.units = rep("npc", 2),
          y.units = rep("npc", 2),
          col = add.line$col,
          lty = add.line$lty,
          lwd = add.line$lwd,
          alpha = add.line$alpha,
          ...)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    ...)
}
\usage{
panel.abline(a, b, \dots)
panel.abline(coef, \dots)
panel.abline(reg, \dots)
panel.abline(h= ,v= , \dots)
panel.curve(expr, from, to, n = 101,
            curve.type = "l", \dots)
panel.rug(x = NULL, y = NULL,
          regular = TRUE, start, end,
          x.units = rep("npc", 2),
          y.units = rep("npc", 2),
          \dots)
panel.fill(col="grey", \dots)
panel.grid(h=3, v=3, \dots)
panel.lmline(x,y,\dots) = panel.abline(lm(y~x), \dots)
panel.loess(x, y, span = 2/3, degree = 1,
            family = c("symmetric", "gaussian"),
            evaluation = 50, \dots)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, \dots)
panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1))
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
  \item{n}{ the number of points to use for drawing the curve }
  \item{regular}{ logical indicating whether `rug' to be drawn on the
    regular side (left / bottom) or not (right / top)
  }
  \item{start, end}{ endpoints of rug segments, in normalized parent
    coordinates (between 0 and 1). Defaults depend on value of regular,
    and cover 3\% of the panel width and height
  }
  \item{x.units, y.units}{
    character vector, replicated to be of length two.  Specifies the
    (grid) units associated with \code{start} and \code{end} above.
    \code{x.units} and \code{y.units} are for the rug on the x-axis and
    y-axis respectively (and thus are associated with \code{start} and
    \code{end} values on the y and x scales respectively).
  }
  \item{from, to}{ optional lower and upper x-limits of curve. If
    missing, limits of current panel are used
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
  \item{\dots}{ graphical parameters can be supplied. see function
    definition for details. Color can usually be specified by
    \code{col}, \code{col.line} and \code{col.symbol}, the last two
    overriding the first for lines and points respectively.
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
  the value of \code{horizontal}), calculates \code{fun} applied to the
  subsets of the other variable determined by each unique value of the
  factor, and joins them by a line. Can be used in conjunction with
  \code{panel.xyplot} and more commonly with panel.superpose to produce
  interaction plots.  See \code{\link{xyplot}} documentation for an
  example.

  \code{panel.mathdensity} plots a (usually theoretical) probability
  density function.  This can be useful in conjunction with
  \code{histogram} and \code{densityplot} to visually estimate goodness
  of fit (note, however, that \code{qqmath} is more suitable for this).

  \code{panel.rug} adds a \emph{rug} representation of the (marginal)
  data to the panel, much like \code{\link{rug}}.

}

\seealso{\code{\link[stats:scatter.smooth]{loess.smooth}},
  \code{\link{panel.axis}}, \code{\link{panel.identify}}
  \code{\link{identify}}, \code{\link{trellis.par.get}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}




