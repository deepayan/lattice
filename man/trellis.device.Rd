\name{trellis.device}
\alias{trellis.device}
\alias{canonical.theme}
\title{ Initializing Trellis Displays }
\description{
  \code{trellis.device} initializes a Trellis display device with
  appropriate settings of the graphics control parameter
  \code{trellis.settings}.
}
\usage{
trellis.device(device = getOption("device"),
               color = !(dev.name == "postscript"),
               theme = getOption("lattice.theme"),
               bg = NULL,
               new = TRUE,
               retain = FALSE,
               ...)
canonical.theme(name, color)
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

    A possible use of this argument is to change the default settings at
    session startup, for example by setting \code{options(lattice.theme
      = "col.whitebg")}. If \code{theme} is a function, it will not be
    supplied any arguments, however, it is guaranteed that a device will
    already be open when it is called, so one may use \code{.Device}
    inside the function to ascertain what device has been opened.
  }
  \item{bg}{ background color for the device.}
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
  \code{.LatticeEnv}) to appropriate (device-specific) values.

  The device specific settings are determined by \code{canonical.theme}
  which returns a list (see below).

  The settings can subsequently be handled by the interface functions
  \code{trellis.par.get}, \code{trellis.par.set} and \code{lset}
}

\value{
  \code{canonical.theme} returns a list of components defining graphical
  parameter settings for Lattice displays. It is used internally in
  \code{trellis.device}, and can also be used as an argument to
  \code{lset}, or even as \code{theme} in \code{trellis.device} to use
  the defaults for another device.
}

\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{trellis.par.set}}, \code{\link{lset}}, 
  \code{\link{postscript}},\code{\link{x11}}, \code{\link{Lattice}}
}
\keyword{dplot}

