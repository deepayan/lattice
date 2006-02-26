\name{panel.bwplot}
\alias{panel.bwplot}
\title{ Default Panel Function for bwplot }
\description{
  This is the default panel function for \code{bwplot}.
}
\usage{
panel.bwplot(x, y, box.ratio = 1,
             horizontal = TRUE,
             pch, col, cex, 
             font, fontfamily, fontface, 
             fill, varwidth = FALSE,
             \dots,
             levels.fos,
             stats = boxplot.stats,
             coef = 1.5,
             do.out = TRUE)
}
\arguments{
  \item{x, y}{ numeric vector or factor. Boxplots drawn for each unique value of
    \code{y} (\code{x}) if \code{horizontal} is \code{TRUE} (\code{FALSE})}
  \item{box.ratio}{ ratio of box height to inter box space}
  \item{horizontal}{
    logical. If FALSE, the plot is \sQuote{transposed} in the sense that
    the behaviours of x and y are switched. x is now the
    \sQuote{factor}.  Interpretation of other arguments change
    accordingly. See documentation of \code{\link{bwplot}} for a fuller
    explanation.
  }
  \item{pch, col, cex, font, fontfamily, fontface}{ graphical parameters
    controlling the dot.  \code{pch="|"} is treated specially, by
    replacing the dot with a line (similar to \code{\link{boxplot}})
  }
  \item{fill}{ color to fill the boxplot}
  \item{varwidth}{ logical. If TRUE, widths of boxplots are proportional
    to the number of points used in creating it.}
  \item{stats}{ a function, defaulting to \code{\link{boxplot.stats}},
    that accepts a numeric vector and returns a list similar to the
    return value of \code{boxplot.stats}.  The function must accept
    arguments \code{coef} and \code{do.out} even if they do not use
    them (a \code{\dots} argument is good enough).  This function is
    used to determine the box and whisker plot.  }
  \item{coef, do.out}{ passed to \code{stats}}
  \item{levels.fos}{ numeric values corresponding to positions of the
    factor or shingle variable.  For internal use. }
  \item{\dots}{
    further arguments, ignored.
  }
}
\details{
  Creates Box and Whisker plot of \code{x} for every level of
  \code{y} (or the reverse if \code{horizontal=FALSE}).  By default, the
  actual boxplot statistics are calculated using \code{boxplot.stats}.
  Note that most arguments controlling the display can be supplied to
  the high-level \code{bwplot} call directly.  Unlike the traditional
  analog \code{\link{boxplot}}, notches are not implemented.
}

\seealso{
  \code{\link{bwplot}}, \code{\link{boxplot.stats}}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{dplot}
