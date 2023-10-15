\name{C_02a_standard.theme}
\alias{standard.theme}
\alias{classic.theme}
\alias{canonical.theme}
\alias{custom_theme}
\alias{col.whitebg}
\title{ Built-in Graphical Themes }
\description{
  Built-in graphical parameter settings. These mainly differ in their
  choice of colors.
}

\usage{
standard.theme(name, color = TRUE,
               symbol = palette.colors(palette = "Okabe-Ito")[c(6, 2, 4, 7, 3, 5, 8)],
               fill   = NULL,
               region = hcl.colors(14, palette = "YlGnBu", rev = TRUE),
               reference = "gray90",
               bg = "transparent",
               fg = "black",
               ...)
canonical.theme(...)
custom_theme(symbol, fill, region,
             reference = "gray90", bg = "transparent", fg = "black",
             strip.bg = rep("gray95", 7), strip.fg = rep("gray70", 7),
             ...)
classic.theme(name, color)
col.whitebg()

}
\arguments{

  \item{name}{ character string giving the name of the device for which
    the setting is required, as returned by \code{.Device}. This is only
    used by \code{classic.theme} to allow device-specific setting. It is
    retained in \code{standard.theme} for back-compatibilty, but its use
    is not recommended.
  }
  \item{color}{ logical, whether the initial settings should be color or
    black and white.
  }

  \item{symbol}{vector of colors to be used for symbols and lines.}
  \item{fill}{vector of colors to be used as fill colors, e.g., in bar
    charts and histograms. The default of \code{NULL} in
    \code{standard.theme} results in lightened versions of the symbol
    colors to be used.
  }
  \item{region}{vector of colors to be used to create a color ramp,
    typically used by \code{\link{levelplot}}
  }
  \item{reference}{ color, to be used for reference lines. }
  \item{fg}{ color, to be used for foreground elements such as axes and
    labels. }
  \item{bg}{ color, to be used as background. }
  \item{strip.bg}{ color, to be used as strip background. }
  \item{strip.fg}{ color, to be used as strip foreground. }
  \item{\dots}{ additional arguments, passed on to other functions as
    appropriate. In particular, additional arguments provided to
    \code{standard.theme} will be passed on to \code{custom_theme}, and
    these may include non-color parameters that will be used to modify
    the resulting theme via \code{\link{simpleTheme}}.
  }
}
\details{

  Trellis Graphics functions obtain the default values of various
  graphical parameters (colors, line types, fonts, etc.) from a
  customizable \dQuote{settings} list (see \code{\link{trellis.par.set}}
  for details).  This functionality is analogous to \code{\link{par}}
  for standard \R graphics and, together with
  \code{\link{lattice.options}}, mostly supplants it (\code{\link{par}}
  settings are mostly ignored by Lattice).  Unlike \code{\link{par}},
  Trellis settings can be controlled separately for each different
  device type (but not concurrently for different instances of the same
  device).

  The functions documented in this page produce such graphical settings
  (a.k.a. themes), usually to be used with \code{\link{trellis.device}}
  or \code{\link{trellis.par.set}}.

  \code{classic.theme} and \code{col.whitebg} produce predefined themes
  that are not recommended for routine use but are retained for
  compatibility.

  The \code{classic.theme} function was intended to provide device
  specific settings (e.g. light colors on a grey background for screen
  devices, dark colors or black and white for print devices) and was
  used to obtain defaults prior to \R 2.3.0.  However, these settings
  are not always appropriate, due to the variety of platforms and
  hardware settings on which \R is used, as well as the fact that a plot
  created on a particular device may be subsequently used in many
  different ways.  For this reason, common device-agnostic defaults were
  used for all devices from \R 2.3.0 onwards.

  Since \R 4.3.0, a new set of defaults given by \code{standard.theme}
  is used. The defaults are based on HCL palettes, but customization of
  the palettes is allowed.  Earlier behaviour can be reinstated by
  setting \code{classic.theme} as the default \code{theme} argument,
  e.g., by putting
  \code{lattice.options(default.theme = classic.theme("pdf"))}
  in a startup script (see the entry for \code{theme} in
  \code{\link{trellis.device}} for details).
  
  \code{custom_theme} is the workhorse function called by
  \code{standard.theme}. \code{canonical.theme} is an alias for
  \code{standard.theme}.
}

\value{

  A list of components defining graphical parameter settings for Lattice
  displays. It is used internally in \code{trellis.device}, and can also
  be used as the \code{theme} argument to \code{trellis.par.set}

  \code{col.whitebg} returns a similar (but smaller) list that is
  suitable as the \code{theme} argument to \code{trellis.device} and
  \code{\link{trellis.par.set}}.  It contains settings values which
  provide colors suitable for plotting on a white background.  Note that
  the name \code{col.whitebg} is somewhat of a misnomer, since it
  actually sets the background to transparent rather than white.
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

  \code{\link{trellis.par.get}} and \code{\link{trellis.par.set}} can be
  used to query and modify the settings \emph{after} a device has been
  initialized.  The \code{par.settings} argument to high level
  functions, described in \code{\link{xyplot}}, can be used to attach
  transient settings to a \code{"trellis"} object.
}
\keyword{dplot}

