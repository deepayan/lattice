\name{trellis.device}
\alias{trellis.device}
\alias{canonical.theme}
\alias{col.whitebg}
\title{ Initializing Trellis Displays }
\description{
  \code{trellis.device} initializes a Trellis display device with
  appropriate settings of the graphics control parameters.
}

\usage{
trellis.device(device = getOption("device"),
               color = !(dev.name == "postscript"),
               theme = lattice.getOption("default.theme"),
               new = TRUE,
               retain = FALSE,
               ...)
canonical.theme(name, color)
col.whitebg()
}
\arguments{
  \item{device}{ name of a function (possibly as a character string)
    that initializes a device. Admissible values include "x11",
    "windows", "mac", "postscript", "pdf", "png", etc.
  }
  \item{color}{ logical, whether the display should be color or black
    and white. \code{FALSE} for postscript devices, \code{TRUE}
    otherwise.
  }
  \item{theme}{ list of components that change the settings of the
    device opened, or, a function that when called produces such a
    list. The function name can be supplied as a quoted string.

    A possible use of this argument is to change the default settings by 
    specifying \code{lattice.options(default.theme = "col.whitebg")}. 
    For back-compatibility, this is initially (when lattice is loaded)
    set to \code{options(lattice.theme)}.

    If \code{theme} is a function, it will not be supplied any
    arguments, however, it is guaranteed that a device will already be
    open when it is called, so one may use \code{.Device} inside the
    function to ascertain what device has been opened.
  }
  \item{new}{ logical flag indicating whether a new device should be
    started. If \code{FALSE},  the options for the current device are
    changed to the defaults determined by the other arguments.
  }
  \item{retain}{
    logical. If \code{TRUE} and a setting for this device already
    exists, then that is used instead of the defaults for this
    device. By default, pre-existing settings are overwritten (and
    lost).
  }
  \item{name}{ name of the device for which the setting is required, as
    returned by \code{.Device}
  }
  \item{\dots}{ additional parameters to be passed to the \code{device}
    function, most commonly \code{file} for non-screen devices.
  }
}
\details{
  \code{trellis.device} calls the \code{device} function to start a
  display, and sets an appropriate component of the variable
  \code{lattice.theme} (in an unexported environment named
  \code{.LatticeEnv}) to appropriate (device-specific) values.  The
  device specific settings are determined by \code{canonical.theme}
  which returns a list (see below).  The settings can subsequently be
  handled by the interface functions \code{trellis.par.get} and 
  \code{trellis.par.set}.


  \code{col.whitebg} returns a similar (but smaller) list that is
  suitable as the \code{theme} argument to \code{trellis.device} and
  \code{\link{trellis.par.set}}.  It contains settings values which
  provide colors suitable for plotting on a white background.  Note that
  the name \code{col.whitebg} is somewhat a misnomer, since it actually
  sets the background to transparent rather than white.

}


\note{

  Trellis Graphics is designed to have a different look and feel on
  different devices.  In particular, the default settings for `screen'
  devices are not at all suitable for printing.

  One technique often used to create printable graphs is through use of
  \code{\link{dev.print}} and related functions (typically via a GUI
  menu).  Unfortunately, this only copies the display already created,
  and hence retains the settings used therein, instead of settings
  suitable for the target device.

  The recommended workaround for this is to re-print the trellis object
  after explicitly starting the new device.  The last printed trellis
  object is typically stored in memory, and it is possible to retrieve
  it using \code{\link{trellis.last.object}}.

  The other option (relevant especially if the trellis plot is enhanced
  interactively after plotting) is to start the initial (screen) device
  with the settings suitable for the eventual target (print)
  device. For instance, a `windows' device can be opened with `pdf'
  settings with

  \code{trellis.device(windows, theme = canonical.theme("pdf"))}

  Note that earlier versions of \code{trellis.device} had a \code{bg}
  argument to set the background color, but this is no longer supported.
  If supplied, the \code{bg} argument will be passed on to the device
  function; however, this will have no effect on the Trellis settings.
  It is rarely meaningful to change the background alone, and if you
  feel the need to change the background, consider using the
  \code{theme} argument.

}


\value{
  \code{canonical.theme} returns a list of components defining graphical
  parameter settings for Lattice displays. It is used internally in
  \code{trellis.device}, and can also be used as the \code{theme}
  argument to \code{trellis.par.set}, or even as \code{theme} in
  \code{trellis.device} to use the defaults for another device.

  \code{col.whitebg} returns a similar but smaller list.
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{trellis.par.set}}, \code{\link{postscript}},
  \code{\link{x11}}, \code{\link{Lattice}}
}
\keyword{dplot}

