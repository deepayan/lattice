\name{B_03_histogram}
\alias{histogram}
\alias{histogram.factor}
\alias{histogram.numeric}
\alias{histogram.formula}
\alias{densityplot}
\alias{densityplot.numeric}
\alias{densityplot.formula}
\alias{do.breaks}
\title{Histograms and Kernel Density Plots}
\usage{
histogram(x, data, \dots)
densityplot(x, data, \dots)
\method{histogram}{formula}(x,
          data,
          allow.multiple, outer = TRUE,
          auto.key = FALSE,
          aspect = "fill",
          panel = lattice.getOption("panel.histogram"),
          prepanel, scales, strip, groups,
          xlab, xlim, ylab, ylim,
          type = c("percent", "count", "density"),
          nint = if (is.factor(x)) nlevels(x)
          else round(log2(length(x)) + 1),
          endpoints = extend.limits(range(as.numeric(x), finite = TRUE), prop = 0.04),
          breaks,
          equal.widths = TRUE,
          drop.unused.levels = lattice.getOption("drop.unused.levels"),
          \dots,
          lattice.options = NULL,
          default.scales = list(),
          subscripts,
          subset)

\method{histogram}{numeric}(x, data = NULL, xlab, \dots)
\method{histogram}{factor}(x, data = NULL, xlab, \dots)


\method{densityplot}{formula}(x,
            data,
            allow.multiple = is.null(groups) || outer,
            outer = !is.null(groups),
            auto.key = FALSE,
            aspect = "fill",
            panel = lattice.getOption("panel.densityplot"),
            prepanel, scales, strip, groups, weights,
            xlab, xlim, ylab, ylim,
            bw, adjust, kernel, window, width, give.Rkern,
            n = 50, from, to, cut, na.rm,
            drop.unused.levels = lattice.getOption("drop.unused.levels"),
            \dots,
            lattice.options = NULL,
            default.scales = list(),
            subscripts,
            subset)
\method{densityplot}{numeric}(x, data = NULL, xlab, \dots)

do.breaks(endpoints, nint)
}
\description{
  Draw Histograms and Kernel Density Plots, possibly conditioned on
  other variables.
}
\arguments{
  \item{x}{

    The object on which method dispatch is carried out.

    For the \code{formula} method, a formula of the form \code{~ x | g1
    * g2 * \dots} indicates that histograms or Kernel density estimates
    of \code{x} should be produced conditioned on the levels of the
    (optional) variables \code{g1, g2, \dots}.  \code{x} can be numeric
    (or factor for \code{histogram}), and each of \code{g1, g2, \dots}
    must be either factors or shingles.

    As a special case, the right hand side of the formula can contain
    more than one term separated by \sQuote{+} signs. What happens in
    this case is described in details in the documentation for
    \code{\link{xyplot}}.  Note that in either form, all the terms in
    the formula must have the same length after evaluation.

    For the \code{numeric} and \code{factor} methods, \code{x} is the
    variable whose histogram or Kernel density estimate is drawn.
    Conditioning is not allowed in these cases.

  }
  \item{data}{
    For the \code{formula} method, an optional data frame in which
    variables are to be evaluated.  Ignored with a warning in other
    cases.
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
    Number of bins.  Applies only when \code{breaks} is unspecified or
    \code{NULL} in the call.  Not applicable when the variable being
    plotted is a factor.
  }
  \item{endpoints}{
    vector of length 2 indicating the range of x-values that is to be
    covered by the histogram.  This applies only when \code{breaks} is
    unspecified and the variable being plotted is not a factor.  In
    \code{do.breaks}, this specifies the interval that is to be divided
    up.
  }
  \item{breaks}{ usually a numeric vector of length (number of bins +
    1) defining the breakpoints of the bins.  Note that when breakpoints
    are not equally spaced, the only value of \code{type} that makes
    sense is density.  When unspecified, the default is to use
    \preformatted{
      breaks = seq_len(1 + nlevels(x)) - 0.5
    }
    when \code{x} is a factor, and 
    \preformatted{
      breaks = do.breaks(endpoints, nint)
    }
    otherwise.  Breakpoints calculated in such a manner are used in all
    panels.

    Other values of \code{breaks} are possible, in which case they
    affect the display in each panel differently.  A special value of
    \code{breaks} is \code{NULL}, in which case the number of bins is
    determined by \code{nint} and then breakpoints are chosen according to
    the value of \code{equal.widths}.  Other valid values of
    \code{breaks} are those of the \code{breaks} argument in
    \code{\link{hist}}.  This allows specification of \code{breaks} as
    an integer giving the number of bins (similar to \code{nint}), as a
    character string denoting a method, and as a function.
  }
  \item{equal.widths}{ logical, relevant only when \code{breaks=NULL}.
    If \code{TRUE}, equally spaced bins will be selected, otherwise, 
    approximately equal area bins will be selected (this would mean that
    the breakpoints will \bold{not} be equally spaced).
  }
  \item{n}{ number of points at which density is to be evaluated }
  \item{panel}{ The function that uses the packet (subset of display
    variables) corresponding to a panel to create a display.  Default
    panel functions are documented separately, and often have arguments
    that can be used to customize its display in various ways.  Such
    arguments can usually be directly supplied to the high level
    function.  }
%   \item{panel.groups}{ function used as the \code{panel.groups} argument
%     when \code{panel.superpose} is used as the panel function, which
%     happens by default when \code{groups} is non-null.  }
  \item{allow.multiple, outer, auto.key, aspect, prepanel, scales,
    strip, groups, xlab, xlim, ylab, ylim, drop.unused.levels,
    lattice.options, default.scales, subscripts, subset}{ See
    \code{\link{xyplot}} }
  \item{weights}{ numeric vector of weights for the density
    calculations, evaluated in the non-standard manner used for
    \code{groups} and terms in the formula, if any.  If this is
    specified, it is subsetted using \code{subscripts} inside the panel
    function to match it to the corresponding \code{x} values.

    At the time of writing, \code{weights} do not work in conjunction
    with an extended formula specification (this is not too hard to fix,
    so just bug the maintainer if you need this feature).
  }
  \item{bw, adjust, kernel, window, width, give.Rkern, from, to, cut,
    na.rm}{ arguments to \code{\link{density}}, passed on as appropriate
  }
  \item{\dots}{ Further arguments.  See corresponding entry in
    \code{\link{xyplot}} for non-trivial details.  }
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

\references{

  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
    Visualization with R}, Springer.
  \url{http://lmdvr.r-forge.r-project.org/}

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
