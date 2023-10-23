pdf("dataframe-methods.pdf")
library(lattice)

mtcars <-
    within(mtcars,
           am <- factor(am, levels = c(0, 1),
                        labels = c("automatic", "manual")))

stripplot(mtcars, am ~ mpg)
stripplot(mtcars, gear ~ mpg | am, jitter = TRUE, grid = TRUE)
bwplot(mtcars, am ~ mpg)
qq(mtcars, am ~ mpg)
qqmath(mtcars, ~ mpg | am)
histogram(mtcars, ~ mpg | am)
densityplot(mtcars, ~ mpg | am)

xyplot(mtcars, mpg ~ wt, groups = am)
cloud(mtcars, mpg ~ wt * disp | am)

splom(mtcars[3:7])
parallelplot(mtcars[3:7])

tmd(mtcars, mpg ~ wt)

ct.tab <- as.data.frame.table(xtabs(~ cyl + am, mtcars))
barchart(ct.tab, cyl ~ Freq, groups = am)
dotplot(ct.tab, cyl ~ Freq, groups = am, auto.key = TRUE)
dotplot(x = mtcars, data = gear ~ mpg, groups = am, auto.key = TRUE)
dotplot(mtcars, formula = gear ~ mpg, col = 1)

g <- expand.grid(a = seq(-1, 1, length.out = 101),
                 b = seq(-1, 1, length.out = 101),
                 KEEP.OUT.ATTRS = FALSE)
g <- within(g, {
    z <- sinh(complex(real = 2 * pi* a, imaginary = 2 * pi * b))
    arg <- Arg(z)
    mod <- Mod(z)
    re <- Re(z)
    im <- Im(z)
})

levelplot(g, arg ~ a * b)
contourplot(g, re ~ a * b)
wireframe(g, im ~ a * b, shade = TRUE)


## check for handling of missing argumnets 
dotplot(mtcars, , formula = gear ~ mpg, col = 1)
dotplot(mtcars, formula = gear ~ mpg, , col = 1)
