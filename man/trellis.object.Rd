\name{trellis.object}
\alias{trellis.object}
\non_function{}
\title{Lattice Display Object}
\description{
  This class of objects is returned by Lattice display functions, and is
  usually plotted by default by its `print' method.
}
\value{
  A ``trellis'' object is a list with the following components:


  \item{aspect.fill}{ logical specifying whether aspect==``fill''}
  \item{aspect.ratio}{ numeric, aspect ratio to be used IF aspect.fill
    is FALSE}
  \item{as.table }{ logical, whether rows are drawn top to bottom}
  \item{call}{ call that generated the object. Not used}
  \item{condlevels}{ Levels of the conditioning variables}
  \item{fontsize.normal}{ default font size for the plot}
  \item{fontsize.small}{ font size for the axis labels (?)}
  \item{key      }{ list defining the key to be drawn}
  \item{layout   }{ layout of the trellis. Must be of length 3, but
    first element can be 0, in which case the second element is taken to
    be the desired number of panels per page and a `nice' layout is
    calculated based on aspect ratio, screen dimensions etc.}
  \item{page     }{ the page function}
  \item{panel    }{ the panel function}
  \item{panel.args}{ a list as long as the number of panels, each
    element being a list itself, containing the arguments in named form
    to be passed to the panel function in that panel. If \code{skip=T}
    for a particular panel, then the corresponding element is
    \code{FALSE} instead of a list.}
  \item{panel.args.common }{ a list containing the arguments common to
    all the panel functions in name=value form}
  \item{par.strip.text }{ list containing graphical parameters
    \code{cex, col, font} for the strip labels}
  \item{skip     }{ logical vector, not used}
  \item{strip    }{ the function to draw the strip labels}
  \item{main     }{ \code{NULL}, or list with components \code{label,
      cex, col, font}}
  \item{sub      }{ same as main}
  \item{xlab     }{ same as sub}
  \item{ylab     }{ same as xlab}
  \item{x.scales }{ list describing x-scale, can consist of several
    other lists, paralleling panel.args, if x-relation is not ``same''}
  \item{y.scales }{ list describing y-scale, same as x.scales}
  \item{x.between}{ numeric vector of interpanel x-space}
  \item{y.between}{ numeric vector of interpanel y-space}
  \item{x.limits}{ numeric vector of length 2 or list, giving x-axis
    limits} 
  \item{y.limits }{ similar to x.limits}
}
\keyword{classes}

