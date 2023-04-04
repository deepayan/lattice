library(lattice)

pdf("fontsize.pdf", pointsize = 5)

splom(iris, sub = "pdf:fontsize = 5")

trellis.par.set(grid.pars = list(fontsize = 5))
splom(iris, sub = "grid.pars:fontsize = 5")

trellis.par.set(grid.pars = list(fontsize = 10))
splom(iris, sub = "grid.pars:fontsize = 10")

trellis.par.set(fontsize = list(text = 15))
splom(iris, sub = "grid.pars:fontsize = 15")

dev.off()


