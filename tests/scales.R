postscript("scales.ps")
## Testing weird scales options

library(lattice)


## text axis colors

xyplot(1:10 ~ 1:10,
       scales =
       list(y = list(font = 2,
            cex = 1,
            col = "green", col.line = "cyan", tck = 5)),
       xlab = list("one to ten",  fontfamily = "HersheySerif"),
       par.settings =
       list(axis.text = list(col = "red", font = 4, cex = 3),
            axis.line = list(col = "yellow")))



## test for POSIXt handling

y <- Sys.time() + 10000 * 1:100
x <- rnorm(100)
b <- gl(3,1,100)

xyplot(y ~ x | b)
xyplot(y ~ x | b, scales = list(relation = "free", rot = 0))
xyplot(y ~ x | b, scales = "sliced")








x <- rnorm(100)
y <- 2 + 3 * runif(100)
a <- gl(3, 1, 100)

xyplot(y ~ x | a)

xyplot(y ~ x | a, scales = list(axs = "i"))

xyplot(y ~ x | a, xlim = c(-5, 5), scales = list(limits = c(-6, 6)))

xyplot(y ~ x | a, xlim = c(-5, 5), ylim = letters[1:5])


## Should produce an error
cat(try(print(xyplot(y ~ x | a, scales = list(x = list( relation = "same", axs = "i", limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ) )))), silent = TRUE))
xyplot(y ~ x | a, scales = list(x = list( relation = "free", axs = "i", limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ) )))
xyplot(y ~ x | a, scales = list(x = list( relation = "sliced", axs = "i", limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ) )))

## Should produce an error
cat(try(print(xyplot(y ~ x | a, xlim = list( c(-5, 5), c(-4, 4), c(-3, 3)  ), scales = list(x = list( relation = "same", axs = "i")))), silent = TRUE))
xyplot(y ~ x | a, xlim = list( c(-5, 5), c(-4, 4), c(-3, 3)  ), scales = list(x = list( relation = "free", axs = "i")))
xyplot(y ~ x | a, xlim = list( c(-5, 5), c(-4, 4), c(-3, 3)  ), scales = list(x = list( relation = "sliced", axs = "i")))




xyplot(y ~ x | a, scales = list(x = list( relation = "free"  )))

xyplot(y ~ x | a, scales = list(x = list( relation = "free", limits = c(-5, 5))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free", axs = "i", limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ) )))

xyplot(y ~ x | a, scales = list(x = list( relation = "free",
                                limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ),
                                at = c(-3, 3, 0))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free",
                                limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), c(-3, 3, 0)  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free",
                                limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), c(-3, 3, 0)  ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], c(-3, 3, 0)  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free",
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), c(-3, 3, 0)  ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], c(-3, 3, 0)  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free",
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), FALSE ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], FALSE  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free",
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], FALSE  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "free", rot = 45,
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], month.name[1:5]  ))))









xyplot(y ~ x | a, scales = list(x = list( relation = "sliced"  )))
xyplot(y ~ x | a, scales = list(x = list( relation = "sliced" , axs = "i" )))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced", limits = c(-5, 5))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced", axs = "i", limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ) )))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced",
                                limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ),
                                at = c(-3, 3, 0))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced",
                                limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), c(-3, 3, 0)  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced",
                                limits = list( c(-5, 5), c(-4, 4), c(-3, 3)  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), c(-3, 3, 0)  ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], c(-3, 3, 0)  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced",
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), c(-3, 3, 0)  ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], c(-3, 3, 0)  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced",
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), FALSE ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], FALSE  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced",
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], FALSE  ))))

xyplot(y ~ x | a, scales = list(x = list( relation = "sliced", rot = 45,
                                limits = list( c(-5, 5), c(-4, 4), letters[1:5]  ),
                                at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ),
                                labels = list( as.character(c(0, 5, -5)) , letters[5:7], month.name[1:5]  ))))




xyplot(y ~ x | a, scales = list(x = list( relation = "free", at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ))))
xyplot(y ~ x | a, scales = list(x = list( relation = "sliced", at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ))))

## should produce an error
cat(try(print(xyplot(y ~ x | a, scales = list(y = list( relation = "same", at = list( c(0, 5, -5) , c(-4, 0, 4), 1:5 ))))), silent = TRUE))


## problem
stripplot(rep(1:20, 5) ~ x | a, scales = list(relation = "free", col = "transparent"))
dev.off()
