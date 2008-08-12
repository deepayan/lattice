\name{Lattice}
\alias{Lattice}
\alias{lattice}
\alias{lattice-package}
\title{Lattice Graphics}
\description{
  Implementation of Trellis Graphics in R
}
\details{
  Trellis Graphics is a framework for data visualization developed at
  the Bell Labs by Rick Becker, Bill Cleveland et al, extending ideas
  presented in Bill Cleveland's 1993 book \emph{Visualizing Data}.

  Lattice is best thought of as an implementation of Trellis Graphics
  for R.  It is built upon the Grid graphics engine and requires the
  \code{grid} add-on package.  It is not (readily) compatible with
  traditional R graphics tools.  The public interface is based on the
  implementation in S-PLUS, but features several extensions, in addition
  to incompatibilities introduced through the use of \code{grid}.  To
  the extent possible, care has been taken to ensure that existing
  Trellis code written for S-PLUS works unchanged (or with minimal
  change) in Lattice. If you are having problems porting S-PLUS code,
  read the entry for \code{panel} in the documentation for
  \code{\link{xyplot}}. Most high level Trellis functions in S-PLUS are
  implemented, with the exception of \code{piechart}.

  Type \code{help(package = lattice)} to see a list of (public)
  Lattice graphics functions for which further documentation is
  available.  The \sQuote{See Also} section below has list of specific
  areas of possible interest and pointers to the help pages with
  respective details.  Apart from the documentation accompanying this
  package, several documents outlining the use of Trellis graphics are
  available from Bell Lab's website; these might provide a holistic
  introduction to the Trellis paradigm.  A book on lattice is also
  available as part of Springer's \sQuote{Use R} series.

  The example section below shows how to bring up a brief history of
  changes to the lattice package, which provides a summary of new
  features.
}

\note{
  High level Lattice functions (like \code{\link{xyplot}}) are different
  from conventional R graphics functions because they don't actually
  draw anything. Instead, they return an object of class
  \code{"trellis"} which has to be then
  \code{\link[lattice:print.trellis]{print}}ed or
  \code{\link[lattice:print.trellis]{plot}}ted to create the actual
  plot.  This is normally done automatically, but not when the high
  level functions are called inside another function (most often
  \code{source}) or other contexts where automatic printing is
  suppressed (e.g. \code{\link{for}} or \code{\link{while}} loops).  In
  such situations, an explicit call to \code{print} or \code{plot} is
  required.

  Lattice plots are highly customizable via user-modifiable settings.
  However, these are completely unrelated to base graphics settings; in
  particular, changing \code{par()} settings usually have no effect on
  lattice plots.
}

\references{

  Sarkar, Deepayan (2008) \dQuote{Lattice: Multivariate Data
    Visualization with R}, Springer.  ISBN: 978-0-387-75968-5
  \url{http://lmdvr.r-forge.r-project.org/}

  Bell Lab's Trellis Page:
  \url{http://cm.bell-labs.com/cm/ms/departments/sia/project/trellis/}

  Cleveland, W.S. (1993) \emph{Visualizing Data}.

  Becker, R.A., Cleveland, W.S. and Shyu, M.
  \dQuote{The Visual Design and Control of Trellis Display},
  \emph{Journal of Computational and Graphical Statistics}

}


\seealso{
  The Lattice user interface primarily consists of several \sQuote{high
    level} generic functions (listed below), each designed to create a
  particular type of statistical display by default.  While each
  function does different things, they share several common features,
  reflected in several common arguments that affect the resulting
  displays in similar ways.  These arguments are extensively (sometimes
  only) documented in the help page for \code{\link{xyplot}}.  This
  includes a discussion of \emph{conditioning} and control of the
  Trellis layout.

  Lattice employs an extensive system of user-controllable parameters to
  determine the look and feel of the displays it produces.  To learn how
  to use and customise the Graphical parameters used by the Lattice
  functions, see \code{\link{trellis.par.set}}.  For other settings, see
  \code{\link{lattice.options}}.  The default graphical settings are
  different for different graphical devices.  To learn how to initialise
  new devices with the desired settings or change the settings of the
  current device, see \code{\link{trellis.device}}.

  To learn about sophisticated (non-default) printing capabilities, see
  \code{\link{print.trellis}}.  See \code{\link{update.trellis}} to
  learn about manipulating a \code{"trellis"} object.  Tools to augment
  lattice plots after they are drawn (including
  \code{\link{locator}}-like functionality) is described in the
  \code{\link{trellis.focus}} help page.
  % \code{\link[lattice:interaction]{interaction}} help page.

  The following is a list of \sQuote{high level} functions in the
  Lattice package with a brief description of what they do.  In all
  cases, the actual display is produced by the so-called \code{panel}
  function, which has a suitable default, but can be substituted by an
  user defined function to create custom displays.  The user will most
  often be interested in the default panel functions, which have a
  separate help page, linked to from the help pages of the corresponding
  high level function.  Although documented separately, arguments to
  these panel functions can be supplied directly to the high level
  functions, which will forward the arguments as appropriate.

  \bold{Univariate:}  

  \code{\link{barchart}} bar plots

  \code{\link{bwplot}} box and whisker plots

  \code{\link{densityplot}} kernel density plots  

  \code{\link{dotplot}} dot plots

  \code{\link{histogram}} histograms

  \code{\link{qqmath}} quantile plots against mathematical distributions

  \code{\link{stripplot}} 1-dimensional scatterplot

  \bold{Bivariate:}

  \code{\link{qq}} q-q plot for comparing two distributions
  
  \code{\link{xyplot}} scatter plot (and possibly a lot more)

  \bold{Trivariate:}

  \code{\link{levelplot}} level plots (similar to image plots in R)

  \code{\link{contourplot}} contour plots

  \code{\link{cloud}} 3-D scatter plots

  \code{\link{wireframe}} 3-D surfaces (similar to persp plots in R)

  \bold{Hypervariate:}
  
  \code{\link{splom}} scatterplot matrix
  
  \code{\link{parallel}} parallel coordinate plots
  
  \bold{Miscellaneous:}

  \code{\link{rfs}} residual and fitted value plot (also see
  \code{\link{oneway}})

  \code{\link{tmd}} Tukey Mean-Difference plot

  Additionally, there are several panel functions that do little by
  themselves, but can be useful components of custom panel functions.
  These are documented in \code{\link{panel.functions}}.  Lattice also
  has a collection of convenience functions that correspond to the base
  graphics primitives \code{\link{lines}}, \code{\link{points}},
  etc.  They are implemented using Grid graphics, but try to be as close
  to the base versions as possible in terms of their argument
  list.  These functions have imaginative names like
  \code{\link{llines}} or \code{\link{panel.lines}} and are often useful
  when writing (or porting from S-PLUS code) nontrivial panel functions.
}

\examples{
\dontrun{
RShowDoc("NEWS", package = "lattice")
}
}

\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
