\name{levelplot}
\title{Level Plots}
\synopsis{
levelplot(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = TRUE,
             aspect = "fill",
             panel = "panel.levelplot",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             at,
             contour = FALSE,
             cuts = 15,
             labels = FALSE,
             pretty = FALSE,
             region = TRUE,
             drop.unused.levels,
             ...,
             default.scales,
             colorkey = region,
             col.regions = trellis.par.get("regions")$col,
             subset = TRUE)
contourplot(formula,
             data = parent.frame(),
             panel = "panel.contourplot",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 7,
             labels = TRUE,
             contour = TRUE,
             pretty = TRUE,
             region = FALSE,
             ...,
             subset = TRUE)
}
\usage{
levelplot(formula, data,
          at,
          contour = FALSE,
          cuts = 15,
          pretty = FALSE,
          region = TRUE,
          ...,
          col.regions = trellis.par.get("regions")$col,
          colorkey = region)
contourplot(formula, data, at,
            contour = TRUE,
            labels = format(at),
            cuts = 7,
            pretty = TRUE,
            ...)

}
\description{
  Draw Level Plots and Contour plots.
}
\alias{levelplot}
\alias{contourplot}
\arguments{
  \item{formula}{a formula of the form \code{z ~ x * y | g1 * g2 * ...},
    where \code{z} is a numeric response, and \code{x, y} are numeric
    values evaluated on a rectangular grid.  \code{g1,g2,...} are
    optional conditional variables, and must be either factors or
    shingles if present.

    Calculations are based on the assumption that all x and y values are
    evaluated on a grid (defined by their unique values). The function
    will not return an error if this is not true, but the display might
    not be meaningful.  However, the x and y values need not be equally
    spaced.

    As an extension to partially support the form used in
    \code{filled.contour} and \code{image}, \code{formula} can be a
    matrix.
  }
  \item{data}{
    optional data frame in which variables are to be evaluated 
  }
  \item{at}{
    numeric vector giving breaks along the range of \code{z}. Contours
    (if any) will be drawn at these heights, and the regions in between
    would be colored using \code{col.regions}.
  }
  \item{col.regions}{ color vector to be used if regions is TRUE. The
    general idea is that this should be a color vector of moderately
    large length (longer than the number of regions. By default this is
    100). It is expected that this vector would be gradually varying in
    color (so that nearby colors would be similar). When the colors are
    actually chosen, they are chosen to be equally spaced along this
    vector. When there are more regions than col.regions, the colors are
    recycled.
  }
  \item{colorkey}{logical specifying whether a color key is to be drawn
    alongside the plot, or a list describing the color key. The list may
    contain the following components:

    \code{space} location of the colorkey, can be one of ``left'',
    ``right'', ``top'' and ``bottom''. Defaults to ``right''.
    
    \code{x,y} location, currently unused

    \code{col} vector of colors

    \code{at} numeric vector specifying where the colors change. must be
    of length 1 more than the col vector.

    \code{labels} a character vector for labelling the \code{at} values,
    or more commonly, a list of components \code{labels, at, cex, col,
      font} describing label positions.

    \code{tick.number} approximate number of ticks.

    \code{corner} interacts with x, y; unimplemented

    \code{width} width of the key

    \code{height} length of key w.r.t side of plot.
  }
  \item{contour}{
    logical, whether to draw contour lines.
  }
  \item{cuts}{
    number of levels the range of \code{z} would be divided into
  }
  \item{labels}{
    logical specifying whether contour lines should be labelled, or
    character vector of labels for contour lines. The type of labelling
    can be controlled by the \code{label.style} argument, which is
    passed on to \code{\link{panel.levelplot}}
  }
  \item{pretty}{
    logical, whether to use pretty cut locations and labels
  }
  \item{region}{
    logical, whether regions between contour lines should be filled
  }
  \item{\dots}{other arguments}
}
\details{
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.

  Other useful arguments are mentioned in the help page for the default
  panel function \code{\link{panel.levelplot}} (these are formally
  arguments to the panel function, but can be specified in the high
  level calls directly).
}
\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
}
\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}},
  \code{\link{panel.levelplot}}
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
x <- seq(pi/4, 5 * pi, length = 100)
y <- seq(pi/4, 5 * pi, length = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE, region = TRUE)

#S-PLUS example
require(stats)
data(environmental)
attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
       parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length = 50)
t.marginal <- seq(min(temperature), max(temperature), length = 50)
r.marginal <- seq(min(radiation), max(radiation), length = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
        radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
contourplot(fit ~ wind * temperature | radiation, data = grid,
          cuts = 10, region = TRUE,
          xlab = "Wind Speed (mph)",
          ylab = "Temperature (F)",
          main = "Cube Root Ozone (cube root ppb)",
          col.regions = trellis.par.get("regions")$col)
detach()
}
\keyword{hplot}



