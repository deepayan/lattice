\name{cloud}
\title{3d Scatter Plot}
\alias{cloud}
\alias{wireframe}
\synopsis{
cloud(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = c(1,1),
             panel = "panel.cloud",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim = if (is.factor(x)) levels(x) else range(x, na.rm = TRUE),
             ylab,
             ylim = if (is.factor(y)) levels(y) else range(y, na.rm = TRUE),
             zlab,
             zlim = if (is.factor(z)) levels(z) else range(z, na.rm = TRUE),
             at,
             drape = FALSE,
             pretty = FALSE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             default.scales = list(distance = c(1, 1, 1), arrows = TRUE, axs = axs.default),
             colorkey = any(drape),
             col.regions,
             alpha.regions,
             cuts = 70,
             subset = TRUE,
             axs.default = "r")
wireframe(formula,
             data = parent.frame(),
             panel = "panel.wireframe",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 70,
             pretty = FALSE,
             drape = FALSE,
             ...,
             colorkey = any(drape),
             subset = TRUE)
}
\usage{
cloud(formula, data,
      aspect = c(1, 1),
      scales = list(distance = rep(1, 3), arrows = TRUE),
      zlab,
      zlim, 
      zoom = 0.8,
      \dots)
wireframe(formula, data,
          at = pretty(z, cuts),
          col.regions,
          alpha.regions,
          drape = FALSE,
          shade = FALSE,
          pretty = FALSE,
          colorkey = any(drape),
          cuts = 70,
          zoom,
          scales,
          \dots,
          col.regions)
}
\description{
  Draws 3d scatter plots and surfaces.
}
\arguments{
  \item{formula}{
    a formula of the form \code{z ~ x * y | g1 * g2 * ...}, where
    \code{z} is a numeric response, and \code{x}, \code{y} are numeric
    values. \code{g1, g2, \dots}, if present, are conditioning variables
    used for conditioning, and must be either factors or shingles. In
    the case of \code{wireframe}, calculations are based on the
    assumption that the \code{x} and \code{y} values are evaluated on a
    rectangular grid defined by their unique values.  The grid points
    need not be equally spaced.

    For \code{wireframe}, \code{x}, \code{y} and \code{z} may also be
    matrices (of the same dimension), in which case they are taken to
    represent a 3-D surface parametrized on a 2-D grid (e.g., a sphere).
    Conditioning is not possible with this feature. See details below.

    Missing values are allowed, either as \code{NA} values in the
    \code{z} vector, or missing rows in the data frame (note however
    that in that case the X and Y grids will be determined only by the
    available values). For a grouped display (producing multiple
    surfaces), missing rows are not allowed, but \code{NA}'s in \code{z}
    are.

    As an extension to partially support the form used in
    \code{filled.contour} and \code{image}, \code{formula} can be a
    matrix.
  }
  \item{data}{
    data frame in which variables are evaluated.  Ignored is
    \code{formula} is not really a formula object.
  }
  \item{aspect}{
    vector of length 2, giving the relative aspects of the y-size/x-size
    and z-size/x-size of the enclosing cube.
  }
  \item{scales}{
    a list describing the scales.  As with other high level functions
    (see \code{\link{xyplot}} for details), this list can contain
    parameters in name=value form.  It can also contain components with
    the special names \code{x}, \code{y} and \code{z}, which can be
    similar lists with axis-specific values overriding the ones
    specified in \code{scales}. 

    The most common use for this argument is to set \code{arrows=FALSE},
    which causes tick marks and labels to be used instead of arrows
    being drawn (the default).  Both can be suppressed by
    \code{draw=FALSE}.

    Other components that work in the \code{scales} argument of
    \code{xyplot} etc. should also work here (as long as they make
    sense), including explicit specification of tick mark locations and
    labels.  (Not everything is implemented yet, but if you find
    something that should work but doesn't, feel free to bug the
    maintainer.)

    Note, however, that for these functions \code{scales} cannot contain
    information that is specific to particular panels.  If you really
    need that, consider using the \code{scales.3d} argument of
    \code{panel.cloud}.
  }
  \item{zlab}{
    label describing the z variable. Similar to \code{xlab} and
    \code{ylab} in other high level functions
  }
  \item{zlim}{
    limits for the z-axis. Similar to \code{xlim} and \code{ylim} in
    other high level functions
  }
  \item{zoom}{
    factor by which to scale the picture. Useful to get the variable
    names into the plot.  This is actually only used in the default
    prepanel function. 
  }
  \item{drape}{
    logical, whether the wireframe is to be draped in color. If
    \code{TRUE}, the height of a facet is used to determine its color in
    a manner similar to the coloring scheme used in
    \code{\link{levelplot}}.  Otherwise, the background color is used to
    color the facets.  This argument is ignored if \code{shade=TRUE}.
  }
  \item{shade}{
    logical, whether the wireframe is to be rendered as being
    illuminated from a light source. See \code{\link{panel.3dwire}} for
    details
  }
  \item{at, col.regions, alpha.regions}{
    these arguments are analogous to those in
    \code{\link{levelplot}}. if \code{drape=TRUE}, \code{at} gives the
    vector of cutpoints where the colors change, and \code{col.regions}
    the vector of colors to be used in that case.  \code{alpha.regions}
    determines the alpha-transparency on supporting devices.  These are
    passed down to the panel function, and also used in the colorkey if
    appropriate.  The default for \code{col.regions} and
    \code{alpha.regions} is derived the Trellis setting ``regions''
  }
  \item{cuts}{
    if \code{at} is unspecified, the approximate number of cutpoints if
    \code{drape=TRUE}
  }
  \item{pretty}{
    whether automatic choice of cutpoints should be prettfied
  }
  \item{colorkey}{
    logical indicating whether a color key should be drawn
    alongside, or a list describing such a key.  See
    \code{\link{levelplot}} for details.
  }
  \item{\dots}{
    other arguments, passed to the panel function. In particular, the
    arguments \code{distance}, \code{perspective}, \code{screen} and
    \code{R.mat} are very important in determining the 3-D
    display. These arguments are described in detail in the help page
    for \code{\link{panel.cloud}}
  }
}
\details{
  These functions produce three dimensional plots in each panel (as long
  as the default panel functions are used).  The orientation is obtained
  as follows: the data are scaled to fall within a bounding box that is
  contained in the [-0.5, 0.5] cube (even smaller for non-default values
  of \code{aspect}).  The viewing direction is given by a sequence of
  rotations specified by the \code{screen} argument, starting from the
  positive Z-axis. The viewing point (camera) is located at a distance
  of \code{1/distance} from the origin. If \code{perspective=FALSE},
  \code{distance} is set to 0 (i.e., the viewing point is at an infinite
  distance).

  \code{cloud} draws a 3-D Scatter Plot, while \code{wireframe} draws a
  3-D surface (usually evaluated on a grid). Multiple surfaces can be
  drawn by \code{wireframe} using the \code{groups} argument (although
  this is of limited use because the display is incorrect when the
  surfaces intersect). Specifying \code{groups} with \code{cloud}
  results in a \code{panel.superpose}-like effect (via
  \code{\link{panel.3dscatter}}).

  \code{wireframe} can optionally render the surface as being
  illuminated by a light source (no shadows though). Details can be
  found in the help page for \code{\link{panel.3dwire}}. Note that
  although arguments controlling these are actually arguments for the
  panel function, they can be supplied to \code{cloud} and
  \code{wireframe} directly.

  For single panel plots, \code{wireframe} can also plot parametrized
  3-D surfaces (i.e., functions of the form f(u,v) = (x(u,v), y(u,v),
  z(u,v)), where values of (u,v) lie on a rectangle. The simplest
  example of this sort of surface is a sphere parametrized by latitude
  and longitude. This can be achieved by calling \code{wireframe} with a
  \code{formula} of the form \code{z~x*y}, where \code{x}, \code{y} and
  \code{z} are all matrices of the same dimension, representing the
  values of x(u,v), y(u,v) and z(u,v) evaluated on a discrete
  rectangular grid (the actual values of (u,v) are irrelevant). 

  When this feature is used, the heights used to calculate \code{drape}
  colors or shading colors are no longer the \code{z} values, but the
  distances of \code{(x,y,z)} from the origin.

  Note that this feature does not work with \code{groups},
  \code{subscripts}, \code{subset}, etc. Conditioning variables are also
  not supported in this case.

  The algorithm for identifying which edges of the bounding box are
  `behind' the points doesn't work in some extreme situations. Also,
  \code{panel.cloud} tries to figure out the optimal location of the
  arrows and axis labels automatically, but can fail on occasion
  (especially when the view is from ``below'' the data). This can be
  manually controlled by the \code{scpos} argument in
  \code{\link{panel.cloud}}.

  These and all other high level Trellis functions have several other
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage. 
}
\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
}
\seealso{
  \code{\link{xyplot}}, \code{\link{levelplot}},
  \code{\link{panel.cloud}},
  \code{\link{Lattice}}  
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\examples{
## volcano  ## 87 x 61 matrix
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

g <- expand.grid(x = 1:10, y = 5:15, gr = 1:2)
g$z <- log((g$x^g$g + g$y^2) * g$gr)
wireframe(z ~ x * y, data = g, groups = gr,
          scales = list(arrows = FALSE),
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 30, x = -60))

cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris,
      screen = list(x = -90, y = 70), distance = .4, zoom = .6)

par.set <-
    list(axis.line = list(col = "transparent"), clip = list(panel = FALSE))
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width, 
            data = iris, cex = .8, 
            groups = Species, 
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 3),
            par.settings = par.set),
      split = c(1,1,2,1), more = TRUE)
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
            data = iris, cex = .8, 
            groups = Species,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 0),
            par.settings = par.set),
      split = c(2,1,2,1))

}
\keyword{hplot}
