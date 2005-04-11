\name{strip.default}
\alias{strip.default}
\alias{strip.custom}
\title{In-built Trellis Strip Function}
\usage{
strip.default(which.given,
              which.panel,
              var.name,
              factor.levels,
              shingle.intervals,
              strip.names = c(FALSE, TRUE),
              style = 1,
              bg = trellis.par.get("strip.background")$col[which.given],
              fg = trellis.par.get("strip.shingle")$col[which.given],
              par.strip.text = trellis.par.get("add.text"))
strip.custom(\dots)
}
\description{
  \code{strip.default} is the function that draws the strips by default
  in conditioning Trellis plots.  Users can write their own strip
  functions, but most commonly this involves calling
  \code{strip.default} with a slightly different arguments.
  \code{strip.custom} provides a convenient way to obtain new strip
  functions that differ from \code{strip.default} only in the default
  values of certain arguments. 
}

\arguments{
  \item{which.given}{ integer index specifying which of the conditioning
    variables this strip corresponds to. 
  }
  \item{which.panel}{ vector of integers as long as the number of
    conditioning variables. The contents are indices specifing the
    current levels of each of the conditioning variables (thus, this
    would be unique for each distinct panel).
  }
  \item{var.name}{ vector of character strings or ecpressions as long as
    the number of conditioning variables. The contents are the names of
    the conditioning variables, to be used if the appropriate entry in
    \code{strip.names} (see below) is true.
  }
  \item{factor.levels}{ if the current strip corresponds to a factor,
    this should be a character or expression vector giving the levels of
    the factor.  Otherwise, it should be \code{NULL}
  }
  \item{shingle.intervals}{if the current strip corresponds to a shingle,
    this should be a 2-column matrix giving the levels of the shingle.
    (of the form that would be produced by \bold{printing}
    \code{levels(shingle)}). Otherwise, it should be \code{NULL}
  }
  \item{strip.names}{ a logical vector of length 2, indicating whether
    or not the name of the conditioning variable that corresponds to the
    strip being drawn is to be written on the strip. The two components
    give the values for factors and shingles respectively.

    This argument is ignored for a factor when \code{style} is not one
    of 1 and 3.
  }
  \item{style}{ integer, with values 1,2,3,4,5 and 6 currently supported.
    Applicable only when \code{x} is a factor. Determines how the current
    level of \code{x} is indicated on the strip.

    The best way to find out what effect the value of \code{style} has
    is to try them out. Here is a short description: for a style value
    of 1, the strip is colored in the background color with the strip
    text (as determined by other arguments) centred on it. A value of 3
    is the same, except that a part of the strip is colored in the
    foreground color, indicating the current level of the factor. For
    styles 2 and 4, the part corresponding to the current level remains
    colored in the foreground color, however, for style = 2, the
    remaining part is not colored at all, whereas for 4, it is colored
    with the background color. For both these, the names of all the
    levels of the factor are placed on the strip from left to right.
    Styles 5 and 6 produce the same effect (they are subtly different in
    S, this implementation corresponds to 5), they are similar to style
    1, except that the strip text is not centred, it is instead
    postioned according to the current level.

    Note that unlike S-PLUS, the default value of \code{style} is 1.
  }
  \item{par.strip.text}{ list with parameters controlling the text on
    each strip, with components \code{col}, \code{cex}, \code{font},
    etc.
  }
  \item{bg}{ strip background color. }
  \item{fg}{ strip foreground color. }
  \item{\dots}{
    arguments to be passed on to \code{strip.default}, overriding
    whatever value it would have normally assumed
  }
}

\value{
  \code{strip.default} is useful for its side-effect, which is to draw
  a strip appropriate for conditioning Trellis plots.
  \code{strip.default} returns a function similar to
  \code{strip.default}

}
  

\details{ default strip function for trellis functions. Useful 
  mostly because of the \code{style} argument --- non-default styles
  are often more informative, especially when the names of the levels
  of the factor \code{x} are small. Typical use is as 
  \code{strip = function(\dots) strip.default(style=2,\dots)}.
}
\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}} 
}

\examples{
xyplot(Petal.Length ~ Petal.Width | Species, iris,
       strip = strip.custom(style = 4))
}

\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}


