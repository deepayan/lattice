\name{splom}
\title{Scatter Plot Matrices}
\synopsis{
splom(formula,
             data = parent.frame(),
             auto.key = FALSE,
             aspect = 1,
             between = list(x = 0.5, y = 0.5),
             panel = if (is.null(groups)) "panel.splom" else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = "Scatter Plot Matrix",
             xlim,
             ylab = NULL,
             ylim,
             superpanel = "panel.pairs",
             pscales = 5,
             varnames,
             drop.unused.levels,
             ...,
             default.scales,
             subset = TRUE)
parallel(formula,
             data = parent.frame(),
             aspect = "fill",
             between = list(x = 0.5, y = 0.5),
             panel = "panel.parallel",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = NULL,
             xlim,
             ylab = NULL,
             ylim,
             varnames,
             drop.unused.levels,
             ...,
             default.scales,
             subset = TRUE)
}
\usage{
splom(formula,
      data,
      aspect = 1,
      between = list(x = 0.5, y = 0.5),
      panel = if (is.null(groups)) "panel.splom" else "panel.superpose",
      superpanel = "panel.pairs",
      pscales = 5,
      varnames, \dots)
parallel(formula,
         data,
         between = list(x = 0.5, y = 0.5),
         panel = "panel.parallel",
         varnames,
         \dots)
}
\description{
  Draw Conditional Scatter Plot Matrices and Parallel Coordinate Plots
}
\alias{splom}
\alias{parallel}
\arguments{
  \item{formula}{ a formula describing the structure of the plot, which
    should be of the form \code{~ x | g1 * g2 * \dots}, where \code{x}
    is a data frame or a matrix. Each of \code{g1,g2,\dots} must be
    either factors or shingles. The conditioning variables
    \code{g1,g2,\dots} may be omitted, in which case the leading
    \code{~} may also be omitted.
  }
  \item{data}{ a data frame containing values for any variables in the
    formula. By default the environment where the function 
    was called from is used.
  }
  \item{aspect}{ 
    aspect ratio of each panel (and subpanel), square by default for
    \code{splom}.
  }
  \item{between}{ 
    to avoid confusion between panels and subpanels, the default is to
    show the panels of a splom plot with space between them.
  }
  \item{panel}{
    Usual interpretation for \code{parallel}, namely the function that
    creates the display within each panel. 

    For \code{splom}, the terminology is slightly complicated. The role
    played by the panel function in most other high-level functions is
    played here by the \code{superpanel} function, which is responsible
    for the display for each conditional data subset. \code{panel} is
    simply an argument to the default \code{superpanel} function
    \code{panel.pairs}, and is passed on to it unchanged. It is used
    there to create each pairwise display. See \code{\link{panel.pairs}}
    for more useful options.

  }
  \item{superpanel}{ 
    function that sets up the splom display, by default as a scatterplot
    matrix.
  }
  \item{pscales}{ 
    a numeric value or a list, meant to be a less functional substitute
    for the \code{scales} argument in \code{xyplot} etc. This argument
    is passed to the \code{superpanel} function, and is handled by the
    default superpanel function \code{panel.pairs}. The help page for
    the latter documents this argument in more detail.
  }
  \item{varnames}{ 
    character vector giving the names of the p variables in x. By
    default, the column names of x.
  }
  \item{\dots}{ other arguments}
}
\details{
  \code{splom} produces Scatter Plot Matrices. The role usually played by
  \code{panel} is taken over by \code{superpanel}, which determines how
  the columns of \code{x} are to be arranged for pairwise plots. The
  only available option currently is \code{panel.pairs}.

  Many of the finer customizations usually done via arguments to high
  level function like \code{xyplot} are instead done by
  \code{panel.pairs} for \code{splom}. These include control of axis
  limits, tick locations and prepanel calcultions. If you are trying to
  fine-tune your \code{splom} plot, definitely look at the
  \code{\link{panel.pairs}} help page. The \code{scales} argument is
  usually not very useful in \code{splom}, and trying to change it may
  have undesired effects.

  \code{parallel} draws Parallel Coordinate Plots. (Difficult to
  describe, see example.)
  
  These and all other high level Trellis functions have several
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
  \code{\link{xyplot}}, \code{\link{Lattice}}, \code{\link{panel.pairs}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(iris)
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Species, data = iris,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                 col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))
splom(~iris[1:3]|Species, data = iris, 
      layout=c(2,2), pscales = 0,
      varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
      page = function(...) {
          ltext(x = seq(.6, .8, len = 4), 
                y = seq(.9, .6, len = 4), 
                lab = c("Three", "Varieties", "of", "Iris"),
                cex = 2)
      })
parallel(~iris[1:4] | Species, iris) 
}
\keyword{hplot}






