

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



## convenient shortcut to create custom strip functions from
## strip.default. Looks a bit dicey, may not always work :-/

strip.custom <-
    function(...)
{
    args <- list(...)
    function(...)
    {
        dots <- list(...)
        do.call("strip.default",
                updateList(dots, args))
    }
}






strip.default <-
    function(which.given,
             which.panel,
             var.name,
             factor.levels,
             shingle.intervals,
             strip.names = c(FALSE, TRUE),
             style = 1,
             ## FIXME: not sure how to incorporate alpha in strip colors
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"))
{
    pushViewport(viewport(y = (which.given-0.5)/length(which.panel),
                          h = 1/length(which.panel),
                          name = paste("strip.default", which.given, sep = ".")))
    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length = 2)
    
    if (is.null(factor.levels)) { # means this is a  shingle, as opposed to a factor
        if (is.null(shingle.intervals))
            stop("both factor.levels and shingle.intervals cannot be NULL")
        strip.names <- strip.names[2]
        grid.rect(gp = gpar(fill = bg, col = bg))
        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level,])-t[1])/diff(t)
        grid.rect(x = unit(r%*%c(.5,.5),"npc"), width = max(unit( c(diff(r), 1), c("npc", "mm"))),
                  gp = gpar(col=fg, fill=fg))
        if (strip.names)
            grid.text(label = name,
                      gp = 
                      gpar(col = par.strip.text$col,
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
    }
    else if (is.null(shingle.intervals)) { # factor
        strip.names <- strip.names[1]
        x <- factor.levels
        num <- length(x)
        if (style == 1) {
            grid.rect(gp = gpar(fill = bg, col = bg))
            if (strip.names) {
                grid.text(name,
                          x=unit(0.5, "npc") - unit(1, "mm"),
                          gp =
                          gpar(col = par.strip.text$col,
                               alpha = par.strip.text$alpha,
                               fontfamily = par.strip.text$fontfamily,
                               fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                               cex = par.strip.text$cex),
                          just="right")
                grid.text(":",
                          x=unit(0.5, "npc"),
                          gp =
                          gpar(col = par.strip.text$col,
                               alpha = par.strip.text$alpha,
                               fontfamily = par.strip.text$fontfamily,
                               fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                               cex = par.strip.text$cex))
                grid.text(x[level],
                          x=unit(0.5, "npc") + unit(1, "mm"),
                          gp =
                          gpar(col = par.strip.text$col,
                               alpha = par.strip.text$alpha,
                               fontfamily = par.strip.text$fontfamily,
                               fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                               cex = par.strip.text$cex),
                          just="left")
            }
            else grid.text(label = x[level],
                           gp =
                           gpar(col = par.strip.text$col,
                                alpha = par.strip.text$alpha,
                                fontfamily = par.strip.text$fontfamily,
                                fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                                cex = par.strip.text$cex))
        }
        else if (style == 2) {
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill = fg, col = fg))
            grid.text(label=x,
                      x = (2*1:num-1)/(2*num),
                      gp =
                      gpar(col = par.strip.text$col,
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
        else if (style == 3){
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill = fg, col = fg))
            grid.text(label =
                      if (strip.names) paste(name, x[level], sep = ": ")
                      else x[level],
                      gp =
                      gpar(col = par.strip.text$col, 
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
        else if(style == 4){
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill = fg, col = fg))
            grid.text(label=x,
                      x = (2* 1:num - 1)/(2*num),   #using default.units
                      gp =
                      gpar(col = par.strip.text$col, 
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
        else if(style >= 5){
            grid.rect(gp = gpar(fill = bg, col = bg))
            grid.text(label=x[level],
                      x = (2* level - 1)/(2*num),   #using default.units
                      gp =
                      gpar(col = par.strip.text$col, 
                           alpha = par.strip.text$alpha,
                           fontfamily = par.strip.text$fontfamily,
                           fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
                           cex = par.strip.text$cex))
        }
    }

    strip.border <- trellis.par.get("strip.border")
    ## draw border for strip
    grid.rect(gp =
              gpar(col = rep(strip.border$col, length = which.given)[which.given],
                   lty = rep(strip.border$lty, length = which.given)[which.given],
                   lwd = rep(strip.border$lwd, length = which.given)[which.given],
                   alpha = rep(strip.border$alpha, length = which.given)[which.given],
                   fill = "transparent"))
    upViewport()
}




