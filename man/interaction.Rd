\name{interaction}
\alias{panel.identify}
\alias{trellis.focus}
\alias{trellis.unfocus}
\alias{trellis.switchFocus}
\alias{trellis.panelArgs}
\alias{trellis.vpname}
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

\synopsis{
trellis.panelArgs(x, panel.number)
trellis.vpname(name =
               c("position", "split", "split.location", "toplevel",
                 "panel", "strip", "legend", "main", "sub",
                 "xlab", "ylab", "page"),
               column = lattice.getStatus("current.focus.column"),
               row = lattice.getStatus("current.focus.row"),
               side = c("left", "top", "right", "bottom", "inside"),
               clip.off = FALSE,
               prefix = lattice.getStatus("current.prefix"))
}

\usage{
panel.identify(x, y = NULL, labels = seq(along = x),
               n = length(x), offset = 0.5,
               threshold = 18,
               panel.args = trellis.panelArgs(),
               \dots)
trellis.vpname(name = c("position", "split", "split.location", "toplevel",
               "panel", "strip", "legend", "main", "sub", "xlab", "ylab", "page"),
               column, row,
               side = c("left", "top", "right", "bottom", "inside"),
               clip.off = FALSE, prefix)
trellis.grobname(name, prefix)
trellis.focus(name, column, row, side, clip.off,
              highlight = interactive(), \dots)
trellis.switchFocus(name, side, clip.off, highlight, \dots)
trellis.unfocus()
trellis.panelArgs()
trellis.panelArgs(x, panel.number)
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
    list that contains components names \code{x} (and usually \code{y}),
    to be used if \code{x} is missing.  Typically, when called after
    \code{trellis.focus}, this would appropriately be the arguments
    passed to that panel.
  }
  \item{name}{
    character string indicating which viewport or grob we are looking
    for. Although these do not necessarily provide access to all
    viewports and grobs created by a lattice plot, they cover most that
    users might find interesting.

    \code{trellis.vpname} and code{trellis.focus} deal with viewport
    names only, and only accept the values explicitly listed above.
    \code{trellis.grobname} is meant to create names for grobs, and can
    currently accept any value.
  }
  \item{column, row}{
    integers, indicating position of the panel or strip that should be
    assigned focus in the Trellis layout. Rows are usually calculated
    from the bottom up, unless the plot was created with
    \code{as.table=TRUE}
  }
  \item{side}{
    character string, relevant only for legends (i.e. when
    \code{name="legend"}), indicating their position.  Partial specification
    is allowed, as long as it is unambiguous.
  }
  \item{clip.off}{
    logical, whether clipping should be off, relevant when \code{name}
    is ``panel'' or ``strip''.  This is necessary if axes are to be
    drawn outside the panel or strip.  Note that setting
    \code{clip.off=FALSE} does not necessarily mean that clipping is on;
    that is determined by conditions in effect during printing.
  }
  \item{prefix}{
    character string acting as a prefix, meant to distinguish otherwise
    equivalent viewports in different plots. This only becomes relevant
    when a particular page is occupied by more than one plot.  Defaults
    to the value appropriate for the last ``trellis'' object printed, as
    determined by the code{prefix} argument in
    \code{\link{print.trellis}}.  Users should not usually need to
    supply a value for this argument (see note below), however, if
    supplied explicitly, this has to be a valid R symbol name (briefly,
    it must start with a letter or a period followed by a letter) and
    must not contain the grid path separator (currently ``::'')
  }
  \item{highlight}{
    logical, whether the viewport being assigned focus should be
    highlighted.  For \code{trellis.focus}, the default is \code{TRUE}
    in interactive mode, and \code{trellis.swich.focus} by default
    preserves the setting currently active.
  }
  \item{panel.number}{
    integer, which panel to get data from.  See \code{\link{xyplot}} for
    details on how this is calculated
  }
  \item{\dots}{
    graphical parameters. For \code{panel.identify} these are used for
    labelling.  For \code{trellis.focus} and \code{trellis.switchFocus},
    these are used (in combination with \code{\link{lattice.options}})
    for highlighting the chosen viewport if so requested.
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

  \code{trellis.focus} can be used to move to a particular panel or
  strip, identified by its position in the array of panels.  It can also
  be used to focus on the viewport corresponding to one of the labels or
  a legend, though such usage would be less useful.  The exact
  viewport is determined by the \code{name} along with the other
  arguments, not all of which are relevant for all names.  Note that
  when more than one object is plotted on a page, \code{trellis.focus}
  will always go to the plot that was created last.  For more
  flexibility, use grid functions directly (see note below).

  After a successful call to \code{trellis.focus}, the desired viewport
  (typically panel or strip area) will be made the `current' viewport
  (plotting area), which can then be enhanced by calls to standard
  lattice panel functions as well as grid functions.

  \code{trellis.unfocus} unsets the focus, and makes the top level
  viewport the current viewport.

  \code{trellis.switchFocus} is a convenience function to switch from
  one viewport to another, while preserving the current \code{row} and
  \code{column}.  Although the rows and columns only make sense for
  panels and strips, they would be preserved even when the user switches
  to some other viewport (where row/column is irrelevant) and then
  switches back.
  
  Once a panel or strip is in focus, \code{trellis.panelArgs} can be
  used to retrieve the arguments that were available to the panel
  function at that position.  \code{trellis.panelArgs} can also, more
  generally, retrieve the panel arguments from any ``trellis'' object.
  Note that for this usage, one needs to specify the \code{panel.number}
  (as described under the \code{panel} entry in \code{\link{xyplot}})
  and not the position in the layout, because a layout determines the
  panel only \bold{after} the object has been printed.

  It is usually not necessary to call \code{trellis.vpname} and
  \code{trellis.grobname} directly.  However, they can be useful in
  generating appropriate names in a portable way when using grid
  functions to interact with the plots directly, as described in the
  note below.

}

\note{

  The viewports created by lattice is accessible to the user only upto a
  certain extent, as described above.  In particular,
  \code{trellis.focus} can only be used to manipulate the last plot
  drawn.  For full flexibility, use appropriate functions from the grid
  package directly.  For example,
  \code{\link[grid:current.viewport]{current.vpTree}} can be used to
  inspect the current viewport tree and
  \code{\link[grid:viewports]{seekViewport}} or
  \code{\link[grid:viewports]{downViewport}} can be used to navigate to
  these viewports.  For such usage, \code{trellis.vpname} and
  \code{trellis.grobname} (with a non-default \code{prefix} argument)
  provides a portable way to access the appropriate viewports and grobs
  by name.

}

\examples{

\dontrun{
xyplot(1:10 ~ 1:10)
trellis.focus(1, 1)
panel.identify()
}

xyplot(Petal.Length ~ Sepal.Length | Species, iris, layout = c(2, 2))
Sys.sleep(1)

trellis.focus("panel", 1, 1)
do.call("panel.lmline", trellis.panelArgs())
Sys.sleep(0.5)
trellis.unfocus()

trellis.focus("panel", 2, 1)
do.call("panel.lmline", trellis.panelArgs())
Sys.sleep(0.5)
trellis.unfocus()

trellis.focus("panel", 1, 2)
do.call("panel.lmline", trellis.panelArgs())
Sys.sleep(0.5)
trellis.unfocus()

}


\seealso{
  \code{\link{identify}}, \code{\link{Lattice}},
  \code{\link{print.trellis}}, 
  \code{\link[grid:current.viewport]{current.vpTree}},
  \code{\link[grid:viewports]{viewports}}
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




