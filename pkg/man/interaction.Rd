\name{interaction}
\alias{panel.identify}
\alias{latticeVP.focus}
\alias{latticeVP.unfocus}
\alias{latticeVP.switch}
\alias{latticeVP.panelArgs}
\title{Functions to Interact with Lattice Plots}
\description{
  The classical Trellis paradigm is to plot the whole object at once,
  without the possibility of interacting with it afterwards.  However,
  by keeping track of the grid viewports where the panels and strips are
  drawn, it is possible to go back to them afterwards and enhance them
  one panel at a time.  These functions provide convenient interfaces to
  help in this.  Note that these are still experimental and the exact
  details may change in future.
}

\usage{
panel.identify(x, y = NULL, labels = seq(along = x),
               n = length(x), offset = 0.5,
               threshold = 18,
               panel.args = latticeVP.panelAgs(),
               \dots)
latticeVP.switch(name = c("panel", "strip"), clip.off = FALSE)
latticeVP.focus(column, row, name = c("panel", "strip"),
                clip.off = FALSE, 
                highlight = interactive(), 
                \dots)
latticeVP.unfocus()
latticeVP.panelArgs()
}

\arguments{
  \item{x, y}{ variables defining the contents of the panel }
  \item{n}{
    for \code{panel.curve}, the number of points to use for drawing the
    curve. For \code{panel.identify}, the number of points to identify
    by default (overridden by a right click)
  }
  \item{labels}{
    in \code{panel.identify}, an optional vector of labels associated
    with each point.
  }
  \item{offset}{
    the labels are printed either below, above, to the left or to the
    right of the identified point, depending on the relative location of
    the mouse click. The \code{offset} specifies (in "char" units) how
    far from the identified point the labels should be printed.
  }
  \item{threshold}{
    threshold in grid's \code{"points"} units. Points further than these
    from the mouse click position are not considered
  }
  \item{panel.args}{
    list that contains components names \code{x} and \code{y}, to be
    used if \code{x} is missing.  Typically, when called after
    \code{latticeVP.focus}, this would appropriately be the arguments
    passed to that panel.
  }
  \item{name}{
    character string indicating whether focus should be set on the panel
    area or the strip area
  }
  \item{clip.off}{
    logical, whether clipping should be off.  This is necessary if axes
    are to be drawn outside the panel or strip.
  }
  \item{column, row}{
    integers, indicating position of the panel or strip that should be
    assigned focus in the Trellis layout. Rows are usually calculated
    from the bottom up, unless the plot was created with
    \code{as.table=TRUE}
  }
  \item{highlight}{
    logical, whether the panel or strip being assigned focus should be
    highlighted.
  } 
  \item{\dots}{
    graphical parameters. For \code{panel.identify} these are used for
    labelling.  For \code{latticeVP.focus}, these are used (in
    combination with \code{\link{lattice.options}}) for highlighting the
    chosen panel or strip.
  }
}

\details{
  \code{panel.identify} is similar to \code{\link{identify}}.  When
  called, it waits for the user to identify points (in the panel being
  drawn) via mouse clicks.  Clicks other than left-clicks terminate the
  procedure. It is less sophisticated than \code{identify} in the sense
  that it does not keep track of points already identified.  Although it
  is possible to call it as part of the panel function, it is more
  typical to use it to identify points after plotting the whole object.


  The process of printing (plotting) a Trellis object builds up a grid
  layout with named viewports which can then be accessed to modify the
  plot further.  While full flexibility can only be obtained by using
  grid functions directly, a few lattice functions are available for the
  more common tasks.

  \code{latticeVP.focus} can be used to seek (in grid terminology) a
  particular panel or strip, identified by its position in the array of
  panels.  After a successful call to \code{latticeVP.focus}, the
  desired panel or strip area will be made the `current' viewport
  (plotting area), which can then be enhanced by calls to standard
  lattice panel functions as well as grid functions.
  \code{latticeVP.unfocus} unsets the focus.

  \code{latticeVP.switch} is a convenience function to switch from a
  panel to the corresponding strip and vice versa, without the need to
  specify the column and rowpositions again.

  Once a panel or strip is in focus, \code{latticeVP.panelArgs} can be
  used to retrieve the arguments that were available to the panel
  function at that position. 
}

\examples{

\dontrun{
xyplot(1:10 ~ 1:10)
latticeVP.focus(1, 1)
panel.identify()
}

xyplot(rnorm(100) ~ rnorm(100))
Sys.sleep(1)
latticeVP.focus(1, 1)
do.call("panel.lmline", latticeVP.panelArgs())
Sys.sleep(1)
latticeVP.unfocus()


}


\seealso{
  \code{\link{identify}}, \code{\link{Lattice}}
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




