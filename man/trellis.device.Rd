\name{C_01_trellis.device}
\alias{trellis.device}
\title{ Initializing Trellis Displays }
\description{
  Initialization of a display device with appropriate graphical
  parameters.
}

\usage{
trellis.device(device = getOption("device"),
               color = !(dev.name == "postscript"),
               theme = lattice.getOption("default.theme"),
               new = TRUE,
               retain = FALSE,
               \dots)
}
\arguments{
  \item{device}{ function (or the name of one as a character string)
    that starts a device.  Admissible values depend on the platform and
    how \R was compiled (see \code{\link{Devices}}), but usually
    \code{"pdf"}, \code{"postscript"}, \code{"png"}, \code{"jpeg"} and
    at least one of \code{"X11"}, \code{"windows"} and \code{"quartz"}
    will be available.
  }
  \item{color}{ logical, whether the initial settings should be color or
    black and white.  Defaults to \code{FALSE} for postscript devices,
    \code{TRUE} otherwise.  Note that this only applies to the initial
    choice of colors, which can be overridden using \code{theme} or
    subsequent calls to \code{\link{trellis.par.set}} (and by arguments
    supplied directly in high level calls for some settings).
  }
  \item{theme}{ list of components that changes the settings of the
    device opened, or, a function that when called produces such a
    list.  The function name can be supplied as a quoted string.  These
    settings are only used to modify the default settings (determined by
    other arguments), and need not contain all possible parameters.

    A possible use of this argument is to change the default settings by 
    specifying \code{lattice.options(default.theme = "col.whitebg")}. 
    For back-compatibility, this is initially (when lattice is loaded)
    set to \code{getOption(lattice.theme)}.

    If \code{theme} is a function, it will not be supplied any
    arguments, however, it is guaranteed that a device will already be
    open when it is called, so one may use \code{.Device} inside the
    function to ascertain what device has been opened.
  }
  \item{new}{ logical flag indicating whether a new device should be
    started.  If \code{FALSE}, the options for the current device are
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
    function, most commonly \code{file} for non-screen devices, as well
    as \code{height}, \code{width}, etc.  See the help file for
    individual devices for admissible arguments.
  }
}
\details{
  The \code{trellis.device} function sets up an R graphics device for
  use with lattice graphics, by opening the device if necessary, and
  defining a set of associated graphical parameters (colors, line types,
  fonts, etc.).

  Even if a device is opened without calling \code{trellis.device}, for
  example, by calling a device function directly, \code{trellis.device}
  is still called automatically when a \code{"trellis"} object is
  plotted. The default graphical settings used in this case can be
  customized using \code{\link{lattice.options}}.  It is therefore
  rarely necessary for the user to call \code{trellis.device}
  explicitly.
}

\value{
  None; \code{trellis.device} is called for the side effect of opening a
  device and / or setting associated graphical parameters.
}

\note{
  Earlier versions of \code{trellis.device} had a \code{bg} argument to
  set the background color, but this is no longer supported.  If
  supplied, the \code{bg} argument will be passed on to the device
  function; however, this will have no effect on the Trellis settings.
  It is rarely meaningful to change the background alone; if you feel
  the need to change the background, consider using the \code{theme}
  argument instead.
}

\references{

  Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
    Visualization with R}, Springer.
  \url{http://lmdvr.r-forge.r-project.org/}

}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{Lattice}} for an overview of the \code{lattice} package.
  
  \code{\link{Devices}} for valid choices of \code{device} on your
  platform.

  \code{\link{standard.theme}} for the default theme and alternatives.
}
\keyword{dplot}

