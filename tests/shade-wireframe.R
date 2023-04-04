
library(lattice)

pdf("shade-wireframe.pdf")

classicShadePalette <- function(irr, ref, height, saturation = .9)
{
    hsv(h = height, s = 1 - saturation * (1 - (1-ref)^0.5), v = irr)
}
trellis.par.set(shade.colors = list(palette = classicShadePalette))
wireframe(volcano, shade = TRUE)

## Using makeShadePalette()

## matte
cm.palette <- makeShadePalette(cm.colors(10), pref = 0.2)
trellis.par.set(shade.colors = list(palette = cm.palette))
wireframe(volcano, shade = TRUE)

## glossy
cm.palette <- makeShadePalette(cm.colors(10), pref = 1.2)
trellis.par.set(shade.colors = list(palette = cm.palette))
wireframe(volcano, shade = TRUE)

ygb.palette <- makeShadePalette(rev(hcl.colors(12, "YlGnBu")))
trellis.par.set(shade.colors = list(palette = ygb.palette))
wireframe(volcano, shade = TRUE)

dev.off()
