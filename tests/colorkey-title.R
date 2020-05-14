postscript("colorkey-title.ps")

library(lattice)

levelplot(volcano) # no title

levelplot(volcano, colorkey = list(title = "Height", space = "bottom"))
levelplot(volcano, colorkey = list(title = "Height", space = "top")) # ??
levelplot(volcano, colorkey = list(title = "Height", space = "left")) # ??
levelplot(volcano, colorkey = list(title = "Height", space = "right"))

levelplot(volcano,
          colorkey = list(space = "bottom",
                          title = list(label = "Height", rot = 0, y = 1, vjust = 0),
                          title.control = list(side = "left", padding = 0.5)))

levelplot(volcano,
          colorkey = list(space = "right",
                          title = list(label = "Height", cex = 1, x = 0, hjust = 0),
                          title.control = list(side = "top")))


require(grid)
f <- frameGrob(layout = grid.layout(heights = unit(0, "mm")))
f <- placeGrob(f, textGrob("Height", x = 0, y = 1, hjust = 0, vjust = -0.5))

levelplot(volcano,
          colorkey = list(space = "right", title = f,
                          title.control = list(side = "top", padding = 0)))

dev.off()
