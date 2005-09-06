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
% \synopsis{
% panel.abline(a, b = NULL, h = numeric(0), v = numeric(0),
%     col,
%     col.line = add.line$col,
%     lty = add.line$lty,
%     lwd = add.line$lwd, 
%     \dots)
% panel.curve(expr, from, to, n = 101,
%             curve.type = "l",
%             col = add.line$col,
%             lty = add.line$lty,
%             lwd = add.line$lwd,
%             type = NULL,
%             \dots)
% panel.rug(x = NULL, y = NULL,
%           regular = TRUE, 
%           start = if (regular) 0 else 0.97,
%           end = if (regular) 0.03 else 1,
%           x.units = rep("npc", 2),
%           y.units = rep("npc", 2),
%           col = add.line$col,
%           lty = add.line$lty,
%           lwd = add.line$lwd,
%           alpha = add.line$alpha,
%           \dots)
% panel.linejoin(x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
%     lty = reference.line$lty, col, col.line = reference.line$col, 
%     \dots)
% }
\usage{

panel.abline(a, b = NULL, h = numeric(0), v = numeric(0),
             col, col.line, lty, lwd, \dots)

panel.curve(expr, from, to, n = 101,
            curve.type = "l",
            col = add.line$col,
            lty = add.line$lty,
            lwd = add.line$lwd,
            type = NULL,
            \dots)
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
          \dots)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    \dots)

% panel.abline(a, b, \dots)
% panel.abline(coef, \dots)
% panel.abline(reg, \dots)
% panel.abline(h= ,v= , \dots)
panel.fill(col, \dots)
panel.grid(h=3, v=3, col, col.line, lty, lwd, \dots)
panel.lmline(x, y, \dots)
panel.loess(x, y, span = 2/3, degree = 1,
            family = c("symmetric", "gaussian"),
            evaluation = 50,
            lwd, lty, col, col.line, \dots)
panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1),
                  n = 50, col, col.line, lwd, lty, \dots)
}
\arguments{
  \item{x, y}{ variables defining the contents of the panel}
  \item{a, b}{ Coefficients of the line to be added.  \code{a} can be a
    vector of length 2, representing the coefficients of the line to be
    added, in which case \code{b} should be missing.  \code{a} can also
    be a suitable \sQuote{regression} object, i.e., an object which has
    a \code{\link{coef}} method that returns a length 2 numeric vector.
    The corresponding line will be plotted.  }

%   \item{coef}{ Coefficients of the line to be added as a length 2 vector}
%   \item{reg}{ A regression object. The corresponding fitted line will
%     be drawn }

  \item{h, v}{ For \code{panel.abline}, numerical vectors giving
    locations respectively of horizontal and vertical lines to be added
    to the plot, in native coordinates.  For \code{panel.grid}, number
    of horizontal and vertical reference lines to be added to the plot;
    \code{h=-1} and \code{v=-1} make the grids aligned with the axis
    labels (but this does not always work).  }
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
  \item{col, col.line, lty, lwd, type, alpha}{ graphical parameters }
  \item{span, degree, family, evaluation}{ arguments to 
    \code{loess.smooth}, around which \code{panel.loess} is essentially
    a wrapper
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

  \code{panel.lmline(x, y)} is equivalent to
  \code{panel.abline(lm(y~x))}.

}

\seealso{\code{\link[stats:scatter.smooth]{loess.smooth}},
  \code{\link{panel.axis}}, \code{\link{panel.identify}}
  \code{\link{identify}}, \code{\link{trellis.par.get}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}




