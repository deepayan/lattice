\name{Lattice}
\alias{Lattice}
\alias{lattice}
\title{Lattice Graphics}
\description{
  Implementation of Trellis Graphics in R
}
\details{
  Trellis Graphics is a framework for data visualization developed at
  the Bell Labs by Rick Becker, Bill Cleveland et al, extending ideas
  presented in Bill Cleveland's 1993 book \emph{Visualizing Data}.

  Lattice is best thought of as an implementation of Trellis Graphics
  for R. Its interface is based on the implementation in S-PLUS,
  but there are several differences. To the extent possible, care has
  been taken to ensure that existing Trellis code written for S-PLUS
  works unchanged (or with minimal change) in Lattice. If you are having
  problems porting S-PLUS code, read the entry for \code{panel} in
  the documentation for \code{\link{xyplot}}. The Lattice website
  referenced below has some useful material as well.

  Most high level Trellis functions in S-PLUS are implemented, with the
  exception of \code{piechart}.

  Lattice is built upon the Grid Graphics engine for R being developed
  by Paul Murrell and requires the \code{grid} add-on package.

  Type \code{library(help = lattice)} to see a list of (public)
  Lattice graphics functions for which further documentation is
  available. Apart from the documentation accompanying this package, 
  several documents outlining the use of Trellis graphics is available
  from Bell Lab's website that might provide a holistic introduction to
  the Trellis paradigm. Lattice also has a website with some tips.
}
\note{
  High level Lattice functions (like \code{\link{xyplot}}) are different
  from conventional S graphics functions because they don't actually
  draw anything. Instead, they return an object of class ``trellis''
  which has to be then \code{print}ed. This often causes confusion when
  the high level functions are called inside another function (most
  often \code{source}) and hence don't produce any output.

  Lattice plots are highly customizable via user-modifiable settings.
  However, these are completely unrelated to base graphics settings; in
  particular, changing \code{par()} settings have no effect on lattice
  plots.
}
\seealso{
  To get a general idea of what arguments are generally accepted by
  Lattice functions, see \code{\link{xyplot}}.

  To learn how to customise the Graphical parameters used by the Lattice
  functions, see \code{\link{trellis.par.set}}.

  To learn how to initialise new devices or change the settings of the
  current device, see \code{\link{trellis.device}}

  To learn about sophisticated (non-default) printing capabilities, see
  \code{\link{print.trellis}}. Also described there are the
  possibilities to enhance the plots thus produced (using \code{locator}
  like functionality).

  Here is a list of `high level' functions in the Lattice library with a
  brief description of what they do:

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

  Lattice also has a collection of convenience functions that correspond
  to the base graphics primitives \code{\link{lines}},
  \code{\link{points}}, etc. They are implemented using Grid graphics,
  but try to be as close to the base versions as possible in terms of
  their argument list. These functions have imaginative names like
  \code{\link{llines}} are are often useful when writing (or porting
  from S-PLUS code) nontrivial panel functions.
}

\references{
  Bell Lab's Trellis Page:
  \url{http://cm.bell-labs.com/cm/ms/departments/sia/project/trellis/}

  Cleveland, W.S. (1993) \emph{Visualizing Data}.

  Becker, R.A., Cleveland, W.S. and Shyu, M.
  ``The Visual Design and Control of Trellis Display'',
  \emph{Journal of Computational and Graphical Statistics}

  Lattice Webpage:
  \url{http://packages.r-project.org/lattice/}
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
