\name{update.trellis}
\alias{update.trellis}
\title{Update Trellis Object}
\description{
  Update method for objects of class ``trellis''
}
\synopsis{
\method{update}{trellis}(object,
       panel,
       aspect,
       as.table,
       between,
       key,
       auto.key,
       legend,
       layout,
       main,
       page,
       par.strip.text,
       prepanel,
       scales,
       skip,
       strip,
       sub,
       xlab,
       xlim,
       ylab,
       ylim,
       par.settings,
       index.cond,
       perm.cond,
       \dots)
}
\usage{
\method{update}{trellis}(object, \dots)
}
\arguments{
  \item{object}{ The object to be updated, of class ``trellis''. }
  \item{\dots}{
    arguments that will be used to update \code{object}. See details
    below. 
  }
}


\details{

  All the high level lattice functions such as \code{xyplot} produce an
  object of (S3) class ``trellis'', which is usually printed by its
  \code{print} method. However, the object itself can be manipulated and
  modified to a large extent using the \code{update} method, and then
  re-plotted as needed.

  Most arguments to high level functions can also be supplied to the
  \code{update} method as well, with some exceptions. Generally
  speaking, anything that would needs to change the data within each
  panel is a no-no (this includes the \code{formula, data, groups,
    subscripts} and \code{subset}). Everything else is technically game,
  though might not be implemented yet. If you find something missing
  that you wish to have, feel free to make a request.

  Not all arguments accepted by a Lattice function are processed by
  \code{update}, but the following should work (they are described in
  the help page for \code{\link{xyplot}}):

  \code{panel}, \code{aspect}, \code{as.table}, \code{between},
  \code{key}, \code{auto.key}, \code{legend}, \code{layout},
  \code{main}, \code{page}, \code{par.strip.text}, \code{prepanel},
  \code{scales}, \code{skip}, \code{strip}, \code{sub}, \code{xlab},
  \code{xlim}, \code{ylab}, \code{ylim}, \code{par.settings},
  \code{index.cond}, \code{perm.cond}

  Any other argument is added to the list of arguments to be passed to
  the \code{panel} function. Because of their somewhat special nature,
  updates to objects produced by \code{cloud} and \code{wireframe} do
  not work very well yet.
}

\value{
  An object of class \code{trellis}, by default plotted by
  \code{print.trellis}.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{trellis.object}}, \code{\link{Lattice}},
  \code{\link{xyplot}}
}
\keyword{dplot}
