\name{qq}
\alias{qq}
\title{ Quantile-Quantile Plots of Two Samples }
\description{
  Quantile-Quantile plots for comparing two Distributions
}
\synopsis{
qq(formula, data = parent.frame(), aspect = "fill", 
    panel = panel.qq, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, f.value = ppoints, 
    drop.unused.levels,
    ..., subscripts = !is.null(groups), subset = TRUE)
}
\usage{
qq(formula, f.value = ppoints, ...)
}
\arguments{
  \item{formula}{formula of the form \code{y ~ x | g1 * g2 * \ldots},
    where \code{x} must be a numeric, and \code{y} can be a factor,
    shingle, character or numeric vector, with the restriction that there
    must be exactly two levels of \code{y}, which divide the values of
    \code{x} into two groups. Quantiles for these groups will be plotted
    along the two axes.
  }
  \item{f.value}{function of a single integer (representing sample
    size), returning a vector of probabilities corresponding to which
    quantiles should be plotted.}
  \item{\dots}{Other arguments}
}
\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\details{
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.
}
\seealso{\code{\link{xyplot}}, \code{\link{panel.qq}},
  \code{\link{Lattice}}
}
\examples{
data(singer)
qq(voice.part ~ height, aspect = 1, data = singer,
   subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))
}
\keyword{dplot}




