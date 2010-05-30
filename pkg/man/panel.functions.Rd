\name{F_2_panel.functions}
\alias{panel.functions}
\alias{panel.abline}
\alias{panel.refline}
\alias{panel.curve}
\alias{panel.rug}
\alias{panel.fill}
\alias{panel.grid}
\alias{panel.lmline}
\alias{panel.loess}
\alias{panel.average}
\alias{panel.linejoin}
\alias{panel.mathdensity}
\title{Useful Panel Functions}
\description{
  These are predefined panel functions available in lattice for use in
  constructing new panel functions (usually on-the-fly).
}
\usage{

panel.abline(a = NULL, b = 0,
             h = NULL, v = NULL,
             reg = NULL, coef = NULL,
             col, col.line, lty, lwd, alpha, type,
             \dots,
             reference = FALSE)
panel.refline(\dots) 

panel.curve(expr, from, to, n = 101,
            curve.type = "l",
            col, lty, lwd, type,
            \dots)
panel.rug(x = NULL, y = NULL,
          regular = TRUE, 
          start = if (regular) 0 else 0.97,
          end = if (regular) 0.03 else 1,
          x.units = rep("npc", 2),
          y.units = rep("npc", 2),
          col, lty, lwd, alpha,
          \dots)
panel.average(x, y, fun = mean, horizontal = TRUE,
              lwd, lty, col, col.line, type,
              \dots)
panel.linejoin(x, y, fun = mean, horizontal = TRUE,
               lwd, lty, col, col.line, type,
               \dots)

% panel.abline(a, b, \dots)
% panel.abline(coef, \dots)
% panel.abline(reg, \dots)
% panel.abline(h= ,v= , \dots)
panel.fill(col, border, \dots)
panel.grid(h=3, v=3, col, col.line, lty, lwd, x, y, \dots)
panel.lmline(x, y, \dots)
panel.loess(x, y, span = 2/3, degree = 1,
            family = c("symmetric", "gaussian"),
            evaluation = 50,
            lwd, lty, col, col.line, type,
            horizontal = FALSE,
            \dots)
panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1),
                  n = 50, col, col.line, lwd, lty, type,
                  \dots)
}
\arguments{
  \item{x, y}{ variables defining the contents of the panel.
    In \code{panel.grid} these are optional and are used only to choose
    an appropriate method of \code{\link{pretty}}.
  }
  \item{a, b}{ Coefficients of the line to be added by
    \code{panel.abline}.  \code{a} can be a vector of length 2,
    representing the coefficients of the line to be added, in which case
    \code{b} should be missing.  \code{a} can also be an appropriate
    \sQuote{regression} object, i.e., an object which has a
    \code{\link{coef}} method that returns a length 2 numeric vector.
    The corresponding line will be plotted.  The \code{reg} argument
    will override \code{a} if specified.
  }
  \item{coef}{ Coefficients of the line to be added as a length 2 vector}
  \item{reg}{ A regression object. The corresponding fitted line will
    be drawn }

  \item{h, v}{ For \code{panel.abline}, these are numeric vectors giving
    locations respectively of horizontal and vertical lines to be added
    to the plot, in native coordinates.  For \code{panel.grid}, these
    usually specify the number of horizontal and vertical reference
    lines to be added to the plot.  Alternatively, they can be negative
    numbers.  \code{h=-1} and \code{v=-1} are intended to make the grids
    aligned with the axis labels.  This doesn't always work; all that
    actually happens is that the locations are chosen using
    \code{pretty}, which is also how the label positions are chosen in
    the most common cases (but not for factor variables,
    for instance).  \code{h} and \code{v} can be negative numbers other
    than \code{-1}, in which case \code{-h} and \code{-v} (as
    appropriate) is supplied as the \code{n} argument to
    \code{\link{pretty}}. }
  \item{reference}{ logical indicating whether the default graphical
    parameters for \code{panel.abline} should be taken from the
    \dQuote{reference.line} parameter settings.  The default is to take
    them from the \dQuote{add.line} settings.  The \code{panel.refline}
    function is a wrapper around \code{panel.abline} that calls it with
    \code{reference=TRUE}.
  }

  \item{expr}{ expression as a function of x or a function to plot as a
    curve }
  \item{n}{ the number of points to use for drawing the curve }
  \item{regular}{ logical indicating whether the \sQuote{rug} is to be
    drawn on the regular side (left / bottom) or not (right / top)
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
  \item{curve.type}{ type of curve (\code{"p"} for points, etc), passed
    to \code{\link{llines}}
  }
  \item{col, col.line, lty, lwd, alpha, border}{ graphical parameters }

  \item{type}{ Usually ignored by the panel functions documented here;
    the argument is present only to make sure an explicitly specified
    \code{type} argument (perhaps meant for another function) doesn't
    affect the display.
  }

  \item{span, degree, family, evaluation}{ arguments to
    \code{loess.smooth}, for which \code{panel.loess} is essentially
    a wrapper.
  }
  \item{fun}{ the function that will be applied to the subset of x(y)
    determined by the unique values of y(x)}
  \item{horizontal}{ logical. If FALSE, the plot is \sQuote{transposed}
    in the sense that the behaviours of x and y are switched. x is now
    the \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
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
  and/or horizontal lines.  Graphical parameters are obtained from the
  \dQuote{add.line} settings by default.  \code{panel.refline} is
  similar, but uses the \dQuote{reference.line} settings for the
  defaults.

  \code{panel.grid} draws a reference grid.

  \code{panel.curve} adds a curve, similar to what \code{curve} does
  with \code{add = TRUE}. Graphical parameters for the line are obtained
  from the \code{add.line} setting.

  \code{panel.average} treats one of x and y as a factor (according to
  the value of \code{horizontal}), calculates \code{fun} applied to the
  subsets of the other variable determined by each unique value of the
  factor, and joins them by a line. Can be used in conjunction with
  \code{panel.xyplot} and more commonly with panel.superpose to produce
  interaction plots.  See \code{\link{xyplot}} documentation for an
  example.  \code{panel.linejoin} is an alias for \code{panel.average}
  retained for back-compatibility and may go away in future.

  \code{panel.mathdensity} plots a (usually theoretical) probability
  density function.  This can be useful in conjunction with
  \code{histogram} and \code{densityplot} to visually estimate goodness
  of fit (note, however, that \code{qqmath} is more suitable for this).

  \code{panel.rug} adds a \emph{rug} representation of the (marginal)
  data to the panel, much like \code{\link{rug}}.

  \code{panel.lmline(x, y)} is equivalent to
  \code{panel.abline(lm(y~x))}.

}

\seealso{\code{\link[stats:scatter.smooth]{loess.smooth}},
  \code{\link{panel.axis}}, \code{\link{panel.identify}}
  \code{\link{identify}}, \code{\link{trellis.par.set}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}




