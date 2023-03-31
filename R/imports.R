
### Copyright (C) 2017-2018 Zhijian (Jason) Wen and Paul Murrell 
### Copyright (C) 2019 Johan Larsson
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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA


## The following functions are modified versions of functions from the
## R package gridGraphics; see
## https://github.com/pmur002/gridgraphics/blob/master/R/filled.contour.R

## vectorization version  (main in used)

FindPolygonVertices <- function(low,  high,
                                x1,  x2,  y1,  y2,
                                z11,  z21,  z12,  z22,
                                colrep)
{
    v1 = FindCutPoints(low, high, x1, y1, x2, y1, z11, z21)
    v2 = FindCutPoints(low, high, y1, x2, y2, x2, z21, z22)
    v3 = FindCutPoints(low, high, x2, y2, x1, y2, z22, z12)
    v4 = FindCutPoints(low, high, y2, x1, y1, x1, z12, z11)
    
    vx = cbind(v1[[1]], v2[[2]], v3[[1]], v4[[2]])
    vy = cbind(v1[[2]], v2[[1]], v3[[2]], v4[[1]])
    
    ##  track the coordinate for x and y( if non-NA's)
    index = rowSums(!is.na(vx) )
    ## keep if non-NAs row >= 2 (npt >= 2)
    vx = t(vx)
    vy = t(vy)
    xcoor.na = as.vector(vx[, index > 2])
    ycoor.na = as.vector(vy[, index > 2])
    ## delete all NA's,
    xcoor = xcoor.na[!is.na(xcoor.na)]
    ycoor = ycoor.na[!is.na(ycoor.na)]
    
    id.length = index[index > 2]
    cols = colrep[index > 2]
    
    out = list(x = xcoor, y = ycoor, id.length = id.length, cols = cols)
    out
}

FindCutPoints <- function(low, high, x1, y1, x2, y2, z1, z2)
{
    ## inner condiction begin
    ## first ocndiction
    c = (z1 - high) / (z1 - z2)
    cond1 = z1 < high
    cond2 = z1 == Inf
    cond3 = z2 > high | z1 < low
    
    x.1 = ifelse(cond1, x1, 
          ifelse(cond2, x2, x1 + c * (x2 - x1)))
    x.1 = ifelse(cond3, NA, x.1)
    
    y.1 = ifelse(cond1, y1, 
          ifelse(cond2, y1, y1))
    y.1 = ifelse(cond3, NA, y.1)
    
    cond4 = z2 == -Inf
    cond5 = z2 <= low
    cond6 = z2 > high | z1 < low
    
    c = (z2 -low) / (z2 - z1)
    x.2 = ifelse(cond4, x1,
          ifelse(cond5, x2 - c * (x2 - x1), NA))
    x.2 = ifelse(cond6, NA, x.2)
    
    y.2 = ifelse(cond4, y1,
          ifelse(cond5, y1, NA))
    y.2 = ifelse(cond6, NA, y.2)
    
    ## second condiction
    cond7 = z1 > low
    cond8 = z1 == -Inf
    cond9 = z2 < low | z1 > high
    
    c = (z1 - low) / (z1 - z2)
    x_1 = ifelse(cond7, x1, 
          ifelse(cond8, x2, x1 + c * (x2 - x1)))
    x_1 = ifelse(cond9, NA, x_1)
    
    y_1 = ifelse(cond7, y1, 
          ifelse(cond8, y1, y1))
    y_1 = ifelse(cond9, NA, y_1)
    
    cond10 = z2 < high
    cond11 = z2 == Inf
    cond12 = z2 < low | z1 > high
    
    c = (z2 - high) / (z2 - z1)
    x_2 = ifelse(cond10, NA, 
          ifelse(cond11, x1, x2 - c * (x2 - x1)))
    x_2 = ifelse(cond12, NA, x_2)
    
    y_2 = ifelse(cond10, NA, 
          ifelse(cond11, y1, y1))
    y_2 = ifelse(cond12, NA, y_2)
    
    ## third condiction
    cond13 = low <= z1 & z1 <= high
    x..1 = ifelse(cond13, x1, NA)
    y..1 = ifelse(cond13, y1, NA)
    ## inner condiction end
    
    ## outer condiction 
    cond14 = z1 > z2
    cond15 = z1 < z2
    
    xout.1 = ifelse(cond14, x.1,
             ifelse(cond15, x_1,
                    x..1))
    xout.2 = ifelse(cond14, x.2,
             ifelse(cond15, x_2,
                    NA))						
    
    yout.1 = ifelse(cond14, y.1,
             ifelse(cond15, y_1,
                    y..1))
    yout.2 = ifelse(cond14, y.2,
             ifelse(cond15, y_2,
                    NA))			
    ## outer condiction end
    
    ## return x1, x2, y1, y2
    xout = cbind(xout.1, xout.2)
    yout = cbind(yout.1, yout.2)
    list(xout, yout)
}

filledContour <- function(x, y, z, s, cols, name, border, lwd, lty, alpha)
{
    ns = length(s)
    nx = length(x)
    ny = length(y)
    
    x1 = rep(x[-nx], each = ny - 1)
    x2 = rep(x[-1], each = ny - 1)
    y1 = rep(y[-ny], nx - 1)
    y2 = rep(y[-1], nx - 1)
    
    z11 = as.numeric(t(z[-nx, -ny]))
    z21 = as.numeric(t(z[-1, -ny ]))
    z12 = as.numeric(t(z[-nx, -1]))
    z22 = as.numeric(t(z[-1, -1]))
    
    x1 = rep(x1, each = ns - 1)
    x2 = rep(x2, each = ns - 1)
    y1 = rep(y1, each = ns - 1)
    y2 = rep(y2, each = ns - 1)
    z11 = rep(z11, each = ns - 1)
    z12 = rep(z12, each = ns - 1)
    z21 = rep(z21, each = ns - 1)
    z22 = rep(z22, each = ns - 1)
    low = rep(s[-ns], (nx - 1) * (ny - 1))
    high = rep(s[-1], (nx - 1) * (ny - 1))
    
    ## rep color until the same length of x, then subsetting 
    if (length(cols) > ns) {
        cols = cols[1:(ns - 1)]
    } else {
        cols = rep_len(cols, ns - 1)
    }
    
    colrep = rep(cols[1:(ns - 1)], nx * ny)
    
    ## feed color as well as subseeting as x and y
    out = FindPolygonVertices(
        low = low, high = high,
        x1 = x1, x2 = x2, 
        y1 = y1, y2 = y2,
        z11 = z11, z21 = z21, 
        z12 = z12, z22 = z22, colrep = colrep)
    ## actual drawing
    
    grid.polygon(out$x, out$y, default.units = 'native',
                 id.lengths = out$id.length,
                 gp = gpar(fill = out$cols, 
                           col = border,
                           lwd = lwd,
                           lty = lty,
                           alpha = alpha),
                 name = name)
}


