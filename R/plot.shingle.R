

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
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







## ## FIXME: not quite what it should be

## plot.shingle.old <-
##     function(x, aspect = "fill", ...)
## {

##     foo <- list(call = match.call(),
##                 aspect.fill = aspect == "fill",
##                 aspect.ratio = if (is.numeric(aspect)) aspect else 1,
##                 as.table = FALSE,
##                 condlevels = "1",
##                 key = NULL,
##                 layout=c(1,1,1),
##                 page = NULL,
##                 panel =
##                 function(x,
##                          col = bar.fill$col,
##                          lty = bar.fill$lty,
##                          lwd = bar.fill$lwd,
##                          alpha = bar.fill$alpha,
##                          border = bar.fill$border,
##                          ...) {
##                     bar.fill <- trellis.par.get("bar.fill")
##                     ## x is the list of intervals
##                     num.l.y <- length(x)
##                     if (num.l.y>0)
##                         for(i in 1:num.l.y)
##                             grid.rect(x = x[[i]] %*% c(.5,.5),
##                                       y = i,
##                                       width = diff(x[[i]]),
##                                       height = .5,
##                                       default.units = "native",
##                                       gp =
##                                       gpar(fill = col,
##                                            alpha = alpha,
##                                            col = border,
##                                            lty = lty,
##                                            lwd = lwd)) 
##                 },
##                 panel.args = list(list()),
##                 panel.args.common = list(x=levels(x), ...),
##                 par.strip.text = trellis.par.get("add.text"),
##                 skip = FALSE,
##                 strip = FALSE,
##                 main = NULL,
##                 sub = NULL,
##                 xlab = c(list(label = "Range"), trellis.par.get("par.xlab.text")),
##                 ylab = c(list(label = "Panel"), trellis.par.get("par.ylab.text")),
##                 x.scales = 1,
##                 y.scales = 1,
##                 x.between = 0,
##                 y.between = 0,
##                 x.alternating = 1,
##                 y.alternating = 1)
    
##     num.l.y <- nlevels(x)
##     foo$x.limits <- extend.limits(range(x, levels(x)))
##     foo$y.limits <- extend.limits(c(1,num.l.y),
##                                   length = .5 + num.l.y)


##     foo$x.scales <- list(relation = "same",
##                          draw = TRUE,
##                          alternating = 1,
##                          at = FALSE,
##                          labels = FALSE,
##                          tck = c(1, 1),
##                          font = 1,
##                          col = FALSE,
##                          log = FALSE,
##                          cex = c(1, 1),
##                          rot = c(FALSE, FALSE),
##                          tick.number = 5)
    
##     foo$y.scales <- list(relation = "same",
##                          draw = TRUE,
##                          alternating = 1,
##                          at = 1:num.l.y,
##                          labels = FALSE,
##                          tck = c(1, 1),
##                          font = 1,
##                          col = FALSE,
##                          log = FALSE,
##                          cex = c(1, 1),
##                          rot = c(FALSE, FALSE),
##                          tick.number = num.l.y)
    
##     class(foo) <- "trellis"
##     foo
    
## }




