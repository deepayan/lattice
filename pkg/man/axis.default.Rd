\name{axis.default}
\alias{axis.default}
\alias{xscale.components.default}
\alias{yscale.components.default}
\title{Default axis annotation utilities}

\description{
  Lattice funtions provide control over how the plot axes are annotated
  through a common interface.  There are two levels of control.  The
  \code{xscale.components} and \code{yscale.components} arguments can be
  functions that determine tick mark locations and labels given a
  packet.  For more direct control, the \code{axis} argument can be a
  function that actually draws the axes.  The functions documented here
  are the defaults for these arguments.  They can additonally be used as
  components of user written replacements.
}

\usage{
xscale.components.default(lim,
                          packet.number = 0,
                          packet.list = NULL,
                          top = TRUE,
                          \dots)
yscale.components.default(lim,
                          packet.number = 0,
                          packet.list = NULL,
                          right = TRUE,
                          \dots)
axis.default(side = c("top", "bottom", "left", "right"),
             scales, components, as.table,
             labels = c("default", "yes", "no"),
             ticks = c("default", "yes", "no"),
             \dots)

}

\arguments{

  \item{lim}{ the range of the data in that packet (data subset
    corresponding to a combination of levels of the conditioning
    variable).  The range is not necessarily numeric; e.g. for factors,
    they could be character vectors representing levels, and for the
    various date-time representations, they could be vectors of length 2
    with the corresponding class. }
  \item{packet.number}{ which packet (counted according to the packet
    order, described in \code{\link{print.trellis}}) is being
    processed.  In cases where all panels have the same limits, this
    function is called only once (rather than once for each packet), in
    which case this argument will have the value \code{0}.
  }
  \item{packet.list}{ list, as long as the number of packets, giving
    all the actual packets.  Specifically, each component is the
    list of arguments given to the panel function when and if that
    packet is drawn in a panel.  (This has not yet been implemented.) }
  \item{top, right}{ the value of the \code{top} and \code{right}
    components of the result, as appropriate.  See below for
    interpretation.}
  \item{side}{ on which side the axis is to be drawn.  The usual partial
    matching rules apply.  }
  \item{scales}{ the appropriate component of the \code{scales} argument
    supplied to the high level function, suitably standardized. }
  \item{components}{ list, similar to those produced by
    \code{xscale.components.default} and
    \code{yscale.components.default}.  }
  \item{as.table}{ the \code{as.table} argument in the high level
    function.  }
  \item{labels}{ whether labels are to be drawn.  By default, the rules
    determined by \code{scales} are used.  }
  \item{ticks}{ whether labels are to be drawn.  By default, the rules
    determined by \code{scales} are used.  }
  \item{\dots}{ many other arguments may be supplied, and are passed on
    to other internal functions. }
}

\value{

  %% FIXME
  
  \code{xscale.components.default} and \code{yscale.components.default}
  return lists in a specific format expected by \code{axis.default}.
  The exact form will be documented later.

}

\details{

  These functions are part of a new API introduced in lattice 0.14 to
  provide the user more control over how axis annotation is done.
  While the API has been designed in anticipation of use that was
  previously unsupported, the implementation has initially focused on
  reproducing existing capabilities, rather then test new features.  At
  the time of writing, several features are unimplemented.  If you
  require them, please contact the maintainer.

}

\examples{

str(xscale.components.default(c(0, 1)))

set.seed(36872)
rln <- rlnorm(100)

densityplot(rln, 
            scales = list(x = list(log = 2), alternating = 3),
            xlab = "Simulated lognormal variates",
            xscale.components = function(...) {
                ans <- xscale.components.default(...)
                ans$top <- ans$bottom
                ans$bottom$labels$labels <- parse(text = ans$bottom$labels$labels)
                ans$top$labels$labels <-
                    if (require(MASS))
                        fractions(2^(ans$top$labels$at))
                    else
                        2^(ans$top$labels$at)
                ans
            })

}


\seealso{
  \code{\link{Lattice}},
  \code{\link{xyplot}}, 
  \code{\link{print.trellis}}
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}

