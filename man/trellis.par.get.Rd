\name{trellis.par.get}
\alias{trellis.par.get}
\alias{trellis.par.set}
\alias{show.settings}
\title{ Graphical Parameters for Trellis Displays }
\description{
  Functions used to query, display and modify graphical parameters for
  fine control of Trellis displays.
}
\usage{
trellis.par.get(name = NULL)
trellis.par.set(name, value, warn)
show.settings(x = NULL)
}
\arguments{
  \item{name}{ character giving the name of a component. For a list of
    valid values type \code{names(trellis.par.get())}.
  }
  \item{value}{ a list giving the desired value of the component. }
  \item{warn}{ logical, whether a warning should be issued when
    \code{trellis.par.get} is called when no graphics device is open }
  \item{x}{ optional list of components that change the settings (any
    valid value of \code{theme} in \code{lset}). These are used to
    modify the current settings (obtained by \code{trellis.par.get})
    before they are displayed
  }
}
\details{
  The various graphical parameters (color, line type, background etc)
  that control the look and feel of Trellis displays are highly
  customizable. Also, R can produce graphics on a number of devices, and
  it is expected that a different set of parameters would be more suited
  to different devices. These parameters are stored internally in a
  variable named \code{lattice.theme}, which is a list whose components
  define settings for particular devices. The components are idenified
  by the name of the device they represent (as obtained by
  \code{.Device}), and are created as and when new devices are opened
  for the first time using \code{trellis.device} (or Lattice plots are
  drawn on a device for the first time in that session).

  The initial settings for each device defaults to values appropriate
  for that device. In practice, this boils down to three distinct
  settings, one for screen devices like \code{x11} and \code{windows},
  one for black and white plots (mostly useful for \code{postscript})
  and one for color printers (color \code{postcript, pdf}). 

  Once a device is open, it's settings can be modified. When another
  instance of the same device is opened later using
  \code{trellis.device}, the settings for that device are reset to its
  defaults, unless otherwise specified in the call to
  \code{trellis.device}. But settings for different devices are treated
  separately, i.e., opening a postscript device will not alter the x11
  settings, which will remain in effect whenever an x11 device is
  active.

  The functions \code{trellis.par.*} are meant to be interfaces to the
  global settings. They always apply on the settings for the currently
  ACTIVE device.

  \code{trellis.par.get}, called without any arguments, returns the full
  list of settings for the active device. With the \code{name} argument
  present, it returns that component only. \code{trellis.par.get} sets
  the value of the \code{name} component of the current active device
  settings to \code{value}.

  \code{trellis.par.get} is usually used inside trellis functions to get
  graphical parameters before plotting. Modifications by users via
  \code{trellis.par.set} is traditionally done as follows:

  \code{add.line <- trellis.par.get("add.line")}

  \code{add.line$col <- "red"}

  \code{trellis.par.set("add.line", add.line)}

  \code{\link{lset}} is a usually more convenient interface to achieve
  the same end (but is not compatible with S-PLUS), the equivalent usage
  being:

  \code{lset(list(add.line = list(col = "red")))}

  The actual list of the components in \code{trellis.settings} has not
  been finalized, so I'm not attempting to list them here. The current
  value can be obtained by \code{print(trellis.par.get())}. Most names
  should be self-explanatory.

  \code{show.settings} provides a graphical display summarizing some of
  the values in the current setting.
}

\value{
  \code{trellis.par.get} returns a list giving parameters for that
  component. If \code{name} is missing, it returns the full list.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  There are some simpler alternatives that can be used to manipulate the
  settings. Details can be found in the documentation for
  \code{\link{lset}}. Other related pages: \code{\link{trellis.device}},
  \code{\link{trellis.par.get}},\code{\link{Lattice}}
}

\examples{
show.settings()
}
\keyword{dplot}


