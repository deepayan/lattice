
## Allow custom widths for barcharts? Currently allowed (without NSE),
## but could be improved.

pdf("barchart-width.pdf")

library(lattice)
x <- 1:10
names(x) <- sample(LETTERS, 10)
barchart(x, origin = 0)

barchart(x, origin = 0, box.width = runif(5))
barchart(x, origin = 0, box.width = runif(5), horizontal = FALSE)

d <- within(expand.grid(a = gl(4, 1), b = gl(10, 1)),
{
    x <- rexp(40)
    w <- 0.8 * runif(40)
})

barchart(b ~ x | a, data = d, origin = 0, box.width = d$w)
barchart(b ~ x | a, data = d, origin = 0, box.width = d$w[1:10])

barchart(b ~ x, data = d, groups = a, box.width = d$w)

barchart(b ~ x, data = d, groups = a, stack = TRUE, box.width = d$w)
barchart(x ~ b, data = d, groups = a, stack = TRUE, box.width = d$w)

dev.off()
