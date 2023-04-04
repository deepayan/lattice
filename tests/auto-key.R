
library(lattice)

pdf("auto-key.pdf", width = 10, height = 7)

d <- data.frame(x = rnorm(100),
                y = rnorm(100),
                a = gl(2, 50),
                b = gl(7, 1, 50))

xyplot(y ~ x | a, data = d, groups = b, type = "p", auto.key = TRUE)

xyplot(y ~ x | a, data = d, groups = b, type = "l", auto.key = TRUE)

xyplot(y ~ x | a, data = d, groups = b, type = "o", auto.key = TRUE)

cloud(y ~ x + a, data = d, groups = b, type = "b", auto.key = TRUE)

splom(~ data.frame(x, y, z = x + y) | a, data = d, groups = b,
      type = "b", auto.key = TRUE)

densityplot(~ x, data = d, groups = b, auto.key = TRUE)

histogram(~ x, data = d, groups = b, auto.key = TRUE)

qqmath( ~ x | a, data = d, groups = b, type = "o", auto.key = TRUE)

barchart(xtabs(x ~ a + b, data = d), auto.key = TRUE)

## not-default placements

xyplot(y ~ x | a, data = d, groups = b, type = "o",
       auto.key = list(space = "left", columns = 2))

xyplot(y ~ x | a, data = d, groups = b, type = "o",
       auto.key = list(columns = 3))

p1 <- xyplot(y ~ x | a, data = d, groups = b, type = "o",
             par.settings = simpleTheme(pch = 16))
p2 <- dotplot(xtabs(x ~ b + a, data = d),
              par.settings = simpleTheme(pch = 16))
p3 <- barchart(xtabs(x ~ b + a, data = d),
               par.settings = simpleTheme(pch = 16))

p1
update(p1, auto.key = list(columns = 3))

p2
update(p2, auto.key = list(columns = 2))

p3
update(p3, auto.key = list(columns = 2))

dev.off()

