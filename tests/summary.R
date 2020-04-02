
library(lattice)

## Output of summary() methods

d <- list(x = 1:10, y = 1:10, g = gl(2, 5))

summary(barchart(factor(y) ~ x, d))     # S3method(barchart, formula)
summary(barchart(unclass(Titanic)))     # S3method(barchart, array)
                                        # S3method(barchart, default)
summary(barchart(VADeaths))             # S3method(barchart, matrix)
summary(barchart(precip))               # S3method(barchart, numeric) 
summary(barchart(Titanic))              # S3method(barchart, table)

summary(bwplot(g ~ x, d))               # S3method(bwplot, formula)
summary(bwplot(precip))                 # S3method(bwplot, numeric)

summary(densityplot( ~ x | g, d))       # S3method(densityplot, formula)
summary(densityplot(precip))            # S3method(densityplot, numeric)

summary(dotplot(factor(y) ~ x, d))      # S3method(dotplot, formula)
summary(dotplot(unclass(Titanic)))      # S3method(dotplot, array)
                                        # S3method(dotplot, default)
summary(dotplot(VADeaths))              # S3method(dotplot, matrix)
summary(dotplot(precip))                # S3method(dotplot, numeric) 
summary(dotplot(Titanic))               # S3method(dotplot, table)


## S3method(histogram,     formula)
## S3method(histogram,     factor)
## S3method(histogram,     numeric)
## S3method(qqmath,        formula)
## S3method(qqmath,        numeric)
## S3method(stripplot,     formula)
## S3method(stripplot,     numeric)
## S3method(qq,            formula)

summary(xyplot(y ~ x | g, data = d))    # S3method(xyplot, formula)
summary(with(d, xyplot(y ~ x | g)))     # S3method(xyplot, formula)
summary(xyplot(sunspot.year))           # S3method(xyplot, ts)

## S3method(levelplot,     formula)
## S3method(levelplot,     table)
## S3method(levelplot,     array)
## S3method(levelplot,     matrix)
## S3method(contourplot,   formula)
## S3method(contourplot,   table)
## S3method(contourplot,   array)
## S3method(contourplot,   matrix)
## S3method(cloud,         formula)
## S3method(cloud,         matrix)
## S3method(cloud,         table)
## S3method(wireframe,     formula)
## S3method(wireframe,     matrix)
## S3method(splom,         formula)
## S3method(splom,         matrix)
## S3method(splom,         data.frame)

## S3method(parallelplot,  formula)
## S3method(parallelplot,  matrix)
## S3method(parallelplot,  data.frame)



        
