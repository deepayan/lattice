library(lattice)

## Background: https://stat.ethz.ch/pipermail/r-devel/2017-May/074245.html

## For a long time, lattice has used the following construct to add a
## $call component to the final "trellis" object produced:

## obj$call <- sys.call(sys.parent()); obj$call[[1]] <- quote(xyplot)

## But this doesn't work in all contexts, especially when using
## with(). From lattice_0.21, this has been changed to use sys.call(),
## but it is important to have this done in EVERY method.

## The following code tests this by checking the call component in
## every high-level method defined in lattice.

g <- data.frame(x = runif(10), y = runif(10), g10 = gl(10, 1), g2 = gl(2, 5))

test.objects <-
    with(g,
         list(barchart.formula = barchart(g10 ~ x | g2, subset = g10 != "1"),
              barchart.array = barchart(unclass(Titanic)),
              barchart.default = barchart(g2),
              barchart.matrix = barchart(VADeaths),
              barchart.numeric = barchart(x),
              barchart.table = barchart(UCBAdmissions),
              bwplot.formula = bwplot(g2 ~ x + y, outer = TRUE),
              bwplot.numeric = bwplot(y, notch = TRUE),
              densityplot.formula = densityplot(~ x, groups = g2),
              densityplot.numeric = densityplot(y, plot.points = "jitter"),
              dotplot.formula = dotplot(g10 ~ x | g2),
              dotplot.array = dotplot(unclass(Titanic)),
              dotplot.default = dotplot(g2),
              dotplot.matrix = dotplot(VADeaths),
              dotplot.numeric = dotplot(x),
              dotplot.table = dotplot(UCBAdmissions),
              histogram.formula = histogram(~ c(x, y)),
              histogram.factor = histogram(g2),
              histogram.numeric = histogram(c(x, y)),
              qqmath.formula = qqmath(~ x + y),
              qqmath.numeric = qqmath(x),
              stripplot.formula = stripplot(g2 ~ x + y, outer = TRUE),
              stripplot.numeric = stripplot(y, jitter = TRUE),
              qq.formula = qq(g2 ~ x),
              xyplot.formula = xyplot(y ~ x),
              xyplot.ts = xyplot(ts(x)),
              levelplot.formula = levelplot(y ~ g2 + g10),
              levelplot.table = levelplot(UCBAdmissions),
              levelplot.array = levelplot(unclass(Titanic)),
              levelplot.matrix = levelplot(VADeaths),
              contourplot.formula = contourplot(y ~ g2 + g10),
              contourplot.table = contourplot(UCBAdmissions),
              contourplot.array = contourplot(unclass(Titanic)),
              contourplot.matrix = contourplot(VADeaths),
              cloud.formula = cloud(g10 ~ x + y),
              cloud.matrix = cloud(VADeaths),
              cloud.table = cloud(UCBAdmissions),
              wireframe.formula = wireframe(y ~ g2 + g10),
              wireframe.matrix = wireframe(VADeaths),
              splom.formula = splom(~cbind(x = x, y = y, g = as.numeric(g2))),
              splom.matrix = splom(cbind(x = x, y = y, g = as.numeric(g2))),
              splom.data.frame = splom(data.frame(x, y, g2)),
              parallelplot.formula = parallelplot(~iris),
              parallelplot.matrix = parallelplot(data.matrix(iris[1:4])),
              parallelplot.data.frame = parallelplot(iris),
              rfs = rfs(oneway(y ~ g2)),
              tmd.formula = tmd(sort(y) ~ sort(x)),
              tmd.trellis = tmd(xyplot(sort(y) ~ sort(x))),
              update.trellis = update(xyplot(y ~ x), pch = 16, cex = 1.5)))
              
## sanity check (some examples without with())

test.objects$xyplot <- xyplot(y ~ x | g2, data = g, cex = c(1, 2))
test.objects$densityplot <- densityplot(g$x, plot.points = FALSE)
test.objects$shingle <- plot(equal.count(rnorm(1000)))


for (m in names(test.objects))
    cat(sprintf("%25s : %s\n", m, paste(deparse(test.objects[[m]]$call), collapse = "")))


pdf("test-call.pdf")
for (m in names(test.objects))
{
    lab <- paste(deparse(test.objects[[m]]$call), collapse = "")
    print(update(test.objects[[m]],
                 page = function(n) panel.text(0.5, 1, labels = lab, pos = 1)))
}
dev.off()

