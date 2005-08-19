\name{histogram}
\alias{histogram}
\alias{histogram.factor}
\alias{histogram.numeric}
\alias{histogram.formula}
\alias{densityplot}
\alias{densityplot.numeric}
\alias{densityplot.formula}
\alias{do.breaks}
\title{Histograms and Kernel Density Plots}
\synopsis{
\method{histogram}{formula}(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = TRUE,
             auto.key = FALSE,
             aspect = "fill",
             panel = "panel.histogram",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             type = c("percent", "count", "density"),
             nint = if (is.factor(x)) length(levels(x))
             else round(log2(length(x)) + 1),
             endpoints = extend.limits(range(x, finite = TRUE), prop = 0.04),
             breaks = if (is.factor(x)) seq(0.5, length = length(levels(x))+1)
             else do.breaks(endpoints, nint),
             equal.widths = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(),
             subscripts = !is.null(groups),
             subset = TRUE)
\method{densityplot}{formula}(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = !is.null(groups),
             auto.key = FALSE,
             aspect = "fill",
             panel = if (is.null(groups)) "panel.densityplot" else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             bw = NULL,
             adjust = NULL,
             kernel = NULL,
             window = NULL,
             width = NULL,
             give.Rkern = FALSE,
             n = 50,
             from = NULL,
             to = NULL,
             cut = NULL,
             na.rm = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(),
             panel.groups = "panel.densityplot",
             subscripts = !is.null(groups),
             subset = TRUE)
}
\usage{
histogram(formula, \dots)
densityplot(formula, \dots)

\method{histogram}{formula}(formula,
          data, 
          type = c("percent", "count", "density"),
          nint = if(is.factor(x)) length(levels(x))
                 else round(log2(length(x))+1),
          endpoints = range(x[!na.x]),
          breaks = if(is.factor(x)) seq(0.5, length = length(levels(x))
          + 1) else do.breaks(endpoints, nint),
          equal.widths = FALSE, 
          \dots)
\method{histogram}{numeric}(formula, \dots)
\method{histogram}{factor}(formula, \dots)

\method{densityplot}{formula}(formula, data, n = 50, plot.points = TRUE, ref = FALSE,
            \dots)
\method{densityplot}{numeric}(formula, \dots)

do.breaks(endpoints, nint)
}
\description{
  Draw Histograms and Kernel Density Plots, possibly conditioned on
  other variables.
}
\arguments{
  \item{formula}{

    For the \code{formula} method, a formula of the form \code{~ x | g1
      * g2 * \dots} indicates that histograms or Kernel Density
    estimates of \code{x} should be produced conditioned on the levels
    of the (optional) variables \code{g1, g2, \dots}.  \code{x} can be
    numeric (or factor for \code{histogram}), and each of \code{g1, g2,
      \dots} must be either factors or shingles.

    As a special case, the right hand side of the formula can contain
    more than one variable separated by a \code{+} sign. What happens in
    this case is described in details in the documentation for
    \code{\link{xyplot}}.  Note that in either form, all the variables
    involved in the formula have to have same length.

    For the \code{numeric} and \code{factor} methods, \code{formula}
    replaces the \code{x} vector described above.  Conditioning is not
    allowed in these cases.

  }
  \item{data}{
    For the \code{formula} method, an optional data frame in which
    variables are to be evaluated.
  }
  \item{type}{
    Character string indicating type of histogram to be drawn.
    \code{"percent"} and \code{"count"} give relative frequency
    and frequency histograms, and can be misleading when breakpoints are
    not equally spaced. \code{"density"} produces a density scale
    histogram.

    \code{type} defaults to \code{"percent"}, except when the
    breakpoints are unequally spaced or \code{breaks = NULL}, when it
    defaults to \code{"density"}.

  }
  \item{nint}{
    Number of bins.  Applies only when \code{breaks} is unspecified in
    the call.
  }
  \item{endpoints}{
    vector of length 2 indicating the range of x-values that is to be
    covered by the histogram. Again, applies only when \code{breaks} is
    unspecified.  In \code{do.breaks}, this specifies the interval that
    is to be divided up.
  }
  \item{breaks}{ numeric vector of length = (number of bins + 1)
    defining the breakpoints of the bins. Note that when breakpoints are
    not equally spaced, the only value of \code{type} that makes sense
    is density.

    Usually all panels use the same breakpoints. This can be changed by
    specifying \code{breaks = NULL}. This has the effect of letting each
    panel choose its own breakpoints. The choice of these breakpoints
    are made as follows: The number of bins is calculated by the formula
    for \code{nint} above, and then breakpoints are chosen according to
    the value of \code{equal.widths}.
  }
  \item{equal.widths}{ logical, relevant only when \code{breaks=NULL}.
    If \code{TRUE}, equally spaced bins will be selected, otherwise, 
    approximately equal area bins will be selected (this would mean that
    the breakpoints will \bold{not} be equally spaced).
  }
  \item{n}{ number of points at which density is to be evaluated }
  \item{plot.points}{
    logical or character string specifying whether (and how) the
    \code{x} values should be plotted along the \code{y=0} line.  This
    argument is passed unchanged to \code{\link{panel.densityplot}},
    whose help page should be consulted for details.
  }
  \item{ref}{
    logical specifying whether a reference x-axis should be drawn.
  }
  \item{\dots}{
    other arguments, passed along to the panel function. In the case of
    \code{densityplot}, if the default panel function is used, then
    arguments appropriate to \code{density} can be included. This can
    control the details of how the Kernel Density Estimates are
    calculated. See documentation for \code{\link{density}} for details.
  }
}
\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}
\details{
  \code{histogram} draws Conditional Histograms, while
  \code{densityplot} draws Conditional Kernel Density Plots.  The
  density estimate in \code{densityplot} is actually calculated using
  the function \code{density}, and all arguments accepted by it can be
  passed (as \code{\dots}) in the call to \code{densityplot} to control
  the output.  See documentation of \code{density} for details. (Note:
  The default value of the argument \code{n} of \code{density} is
  changed to 50.)
  
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.

  \code{do.breaks} is an utility function that calculates breakpoints
  given an interval and the number of pieces to break it into.
  
}
\note{
  The form of the arguments accepted by the default panel function
  \code{panel.histogram} is different from that in S-PLUS. Whereas
  S-PLUS calculates the heights inside \code{histogram} and passes only
  the breakpoints and the heights to the panel function, here the
  original variable \code{x} is passed along with the breakpoints. This
  allows plots as in the second example below.
}

\seealso{
  \code{\link{xyplot}},
  \code{\link{panel.histogram}},
  \code{\link{density}},
  \code{\link{panel.densityplot}},
  \code{\link{panel.mathdensity}},
  \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
require(stats)
histogram( ~ height | voice.part, data = singer, nint = 17,
          endpoints = c(59.5, 76.5), layout = c(2,4), aspect = 1,
          xlab = "Height (inches)")

histogram( ~ height | voice.part, data = singer,
          xlab = "Height (inches)", type = "density",
          panel = function(x, ...) {
              panel.histogram(x, ...)
              panel.mathdensity(dmath = dnorm, col = "black",
                                args = list(mean=mean(x),sd=sd(x)))
          } )

densityplot( ~ height | voice.part, data = singer, layout = c(2, 4),  
            xlab = "Height (inches)", bw = 5)
}
\keyword{hplot}
