

### Copyright 2001-2004 Deepayan Sarkar <deepayan@stat.wisc.edu> and 
###
### This file is part of the lattice library for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA




"[.shingle" <-
    function(x, subset, drop = FALSE)
{
    if (!is.shingle(x)) stop("x must be a shingle")
    ans <- as.numeric(x)[subset]
    attr(ans, "levels") <- levels(x)
    class(attr(ans, "levels")) <- "shingleLevel"
    if (drop) {
        xlvs <- levels(ans)
        dl <- logical(nlevels(ans))
        for (i in seq(along=dl))
            dl[i] <- any( ans >= xlvs[[i]][1] & ans <= xlvs[[i]][2] )
        attr(ans, "levels") <- xlvs[dl]
        class(attr(ans, "levels")) <- "shingleLevel"
    }
    class(ans) <- "shingle"
    ans
}





make.list.from.intervals <- function(x)
{
    if (ncol(x)!=2) stop("x must be matrix with 2 columns")
    if (nrow(x)<1) stop("x must be matrix with at least 1 row")
    ans <- as.list(1:nrow(x))
    for (i in 1:nrow(x))
        ans[[i]] <- x[i,]
    ans
}



equal.count <-
    function(x, ...)
{
    attr(x, "levels") <- make.list.from.intervals(co.intervals(x,...))
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}



shingle <-
    function(x, intervals=sort(unique(x)))
{
    if (ncol(as.matrix(intervals))==1)
        intervals <- cbind(intervals, intervals)
    else if (ncol(as.matrix(intervals)) > 2)
        stop("bad value of 'intervals'")
    attr(x, "levels") <- make.list.from.intervals(intervals)
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}


as.data.frame.shingle <- as.data.frame.factor

is.shingle <-
    function(x) inherits(x, "shingle")


as.shingle <-
    function(x) if (is.shingle(x)) x else shingle(x)



summary.shingle <- function(object, ...) print.shingle(object, ...)




print.shingleLevel <-
    function(x, ...) {
        print(do.call("rbind", x))
        invisible(x)
    }

print.shingle <- function(x, showValues = TRUE, ...) {
    cat("\nData:\n")
    if (showValues) print(as.numeric(x))
    l <- levels(x)
    n <- nlevels(x)
    if (n<1) cat("\nno intervals\n")
    else {
        int <- data.frame(min = numeric(n), max = numeric(n), count = numeric(n))
        for (i in 1:n) {
            int$min[i] <- l[[i]][1]
            int$max[i] <- l[[i]][2]
            int$count[i] <- length(x[x>=l[[i]][1] & x<=l[[i]][2]])
        }
        cat("\nIntervals:\n")
        print(int)
        olap <- numeric(n-1)
        if (n>2)
            for (i in 1:(n-1))
                olap[i] <- length(x[ x>=l[[i]][1] & x<=l[[i]][2] &
                                    x>=l[[i+1]][1] & x<=l[[i+1]][2]])
        cat("\nOvrlap between adjacent intervals:\n")
        print(olap)
    }
    invisible(x)
}


