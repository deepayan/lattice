




### Copyright 2001-2002  Deepayan Sarkar <deepayan@stat.wisc.edu>
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








## Plan: to make things more modular than they are now. As a first
## step, get a function that does a 3d transformation. Probably a good
## idea to do things in terms of homogeneous coordinates


ltransform3dMatrix <- function(screen, R.mat = diag(4)) {

    rot.mat <- diag(3)
    screen.names <- names(screen)
    screen <- lapply(screen, "*", pi/180)

    for(i in seq(along=screen.names)) {
        th <- screen[[i]]
        cth <- cos(th)
        sth <- sin(th)
        tmp.mat <-
            (if (screen.names[i]=="x")
             matrix(c(1, 0, 0, 0, cth, sth, 0, -sth, cth), 3, 3)
            else if (screen.names[i]=="y")
             matrix(c(cth, 0, -sth, 0, 1, 0, sth, 0, cth), 3, 3)
            else if (screen.names[i]=="z")
             matrix(c(cth, sth, 0, -sth, cth, 0, 0, 0, 1), 3, 3))
        rot.mat <- tmp.mat %*% rot.mat
    }
    rot.mat <- cbind(rot.mat, c(0,0,0))
    rot.mat <- rbind(rot.mat, c(0,0,0,1))
    if (!missing(R.mat)) rot.mat <- rot.mat %*% R.mat
    rot.mat
}





ltransform3dto3d <- function(x, R.mat, dist = 0) {

    if (length(x) == 0) return(x)
    tdata <- R.mat %*% rbind(x, 1)

    ## back to 3d
    tdata[1,] <- tdata[1,]/tdata[4,]
    tdata[2,] <- tdata[2,]/tdata[4,]
    tdata[3,] <- tdata[3,]/tdata[4,]

    ## now 'perspective' x,y coordinates. z remains unmodified
    if (dist != 0)  ## 1/dist = distance of eye from center
    {
        tdata[1,] <- tdata[1,] / (1/dist - tdata[3,])
        tdata[2,] <- tdata[2,] / (1/dist - tdata[3,])
    }

    tdata[1:3, ]
}










prepanel.default.cloud <-
    function(distance = 0, xlim, ylim, zlim, zoom = 1,
             rot.mat, aspect, ...)
{

    aspect <- rep(aspect, length=2)
    corners <-
        rbind(x = c(-1,1,1,-1,-1,1,1,-1),
              y = c(-1,-1,-1,-1,1,1,1,1) * aspect[1],
              z = c(-1,-1,1,1,-1,-1,1,1) * aspect[2])
    corners <- corners / (2 * max(corners)) ## contain in [-.5, .5] cube
    corners <- ltransform3dto3d(corners, rot.mat, dist = distance)

    xrng <- range(corners[1,])
    yrng <- range(corners[2,])
    slicelen <- max(diff(xrng), diff(yrng))

    list(xlim = extend.limits(xrng, length = slicelen) / zoom,
         ylim = extend.limits(yrng, length = slicelen) / zoom,
         dx = 1, dy = 1)
}




panel.3dscatter <-
    function(x, y, z, rot.mat = diag(4), distance,
             groups = NULL,
             type = 'p',
             xlim.scaled,
             ylim.scaled,
             zlim.scaled,
             zero.scaled,
             col,
             ## eventually make all these cloud.3d$col etc
             col.point = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
             col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
             lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
             lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
             cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
             pch = if (is.null(groups)) "+" else superpose.symbol$pch,
             cross,
             ...,
             subscripts = TRUE)
{

    ##cloud.3d <- list(col=1, cex=1, lty=1, lwd=1, pch=1)
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col))
    {
        col.point <- col
        col.line <- col
    }

    n <- length(x)
    if (n > 0)
    {
        if (is.null(groups))
        {
            col.point <- rep(col.point, length = n)
            col.line <- rep(col.line, length = n)
            lty <- rep(lty, length = n)
            lwd <- rep(lwd, length = n)
            cex <- rep(cex, length = n)
            pch <- rep(pch, length = n)
        }
        else
        {
            nvals <- nlevels(as.factor(groups))
            groups <- as.numeric(groups[subscripts])

            col.point <- rep(col.point, length = nvals)[groups]
            col.line <- rep(col.line, length = nvals)[groups]
            lty <- rep(lty, length = nvals)[groups]
            lwd <- rep(lwd, length = nvals)[groups]
            cex <- rep(cex, length = nvals)[groups]
            pch <- rep(pch, length = nvals)[groups]
        }


        ## The following code deals with different 'type's. (We allow
        ## for multiple types, but do them sequentially, so
        ## overplotting may happen. The amount of work required to fix
        ## this is too much to make it worth the effort.)

        ## 'points' type
        if (any(c('p', 'b', 'o') %in% type))
        {
            id <-
                ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) &
                 (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) &
                 (z >= zlim.scaled[1]) & (z <= zlim.scaled[2]) &
                 !is.na(x) & !is.na(y) & !is.na(z))

            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            ord <- sort.list(m[3,])
            ord <- ord[id[ord]]

            if (missing(cross)) cross <- all(pch == "+")
            if (cross) ## plot symbols are 3d cross hairs
            {
                tmpx0 <- rep(x[ord], each = 3) - rep(cex[ord], each = 3) * c(0.02, 0, 0)
                tmpx1 <- rep(x[ord], each = 3) + rep(cex[ord], each = 3) * c(0.02, 0, 0)
                tmpy0 <- rep(y[ord], each = 3) - rep(cex[ord], each = 3) * c(0, 0.02, 0)
                tmpy1 <- rep(y[ord], each = 3) + rep(cex[ord], each = 3) * c(0, 0.02, 0)
                tmpz0 <- rep(z[ord], each = 3) - rep(cex[ord], each = 3) * c(0, 0, 0.02)
                tmpz1 <- rep(z[ord], each = 3) + rep(cex[ord], each = 3) * c(0, 0, 0.02)

                m0 <- ltransform3dto3d(rbind(tmpx0, tmpy0, tmpz0), rot.mat, distance)
                m1 <- ltransform3dto3d(rbind(tmpx1, tmpy1, tmpz1), rot.mat, distance)

                lsegments(x0 = m0[1,], y0 = m0[2,],
                          x1 = m1[1,], y1 = m1[2,],
                          col = rep(col.line[ord], each = 3))
            }
            else
            {
                lpoints(x = m[1, ord], y = m[2, ord],
                        col = col.point[ord],
                        pch = pch[ord],
                        cex = cex[ord])
            }
        }


        ## 'lines' type
        if (any(c('l', 'b', 'o') %in% type))
        {
            ord <- if (is.null(groups)) TRUE else sort.list(groups)
            tmplen <- length(x)
            tmpx0 <- x[ord][-1]
            tmpx1 <- x[ord][-tmplen]
            tmpy0 <- y[ord][-1]
            tmpy1 <- y[ord][-tmplen]
            tmpz0 <- z[ord][-1]
            tmpz1 <- z[ord][-tmplen]
            tmpcol0 <- col.line[ord][-1]
            tmpcol1 <- col.line[ord][-tmplen]

            tmpcol0[tmpcol0 != tmpcol1] <- "transparent"

            m0 <- ltransform3dto3d(rbind(tmpx0, tmpy0, tmpz0), rot.mat, distance)
            m1 <- ltransform3dto3d(rbind(tmpx1, tmpy1, tmpz1), rot.mat, distance)

            ## a collection of line segments in 3d space is not well
            ## ordered. This is just a naive heuristic:

            ord <- sort.list(pmax(m0[3,], m1[3,]))

            lsegments(x0 = m0[1, ord], y0 = m0[2, ord],
                      x1 = m1[1, ord], y1 = m1[2, ord],
                      col = tmpcol0[ord])
        }


        ## 'histogram' type
        if ('h' %in% type)
        {
            id <-
                ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) &
                 (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) &
                 !is.na(x) & !is.na(y) & !is.na(z))

            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            ord <- sort.list(m[3,])
            ord <- ord[id[ord]]
            zero.scaled <-
                if (zero.scaled < zlim.scaled[1]) zlim.scaled[1]
                else if (zero.scaled > zlim.scaled[2]) zlim.scaled[2]
                else zero.scaled
            other.end <- ltransform3dto3d(rbind(x, y, zero.scaled), rot.mat, distance)
            lsegments(m[1,ord], m[2,ord],
                      other.end[1,ord], other.end[2,ord],
                      col = col.line[ord],
                      lty = lty[ord],
                      lwd = lwd[ord])
        }

        if (any(!(type %in% c('p', 'h', 'l', 'b', 'o'))))
        {
            warning("type has unsupported values")
        }
    }
}




####################################################################
##                     Interface to C code                        ##
####################################################################


## the following is now part of the settings

# palette.shade <-
#     function(irr, ref, height, saturation = .9)
# {
#     hsv(h = height,
#         s = 1 - saturation * (1 - (1-ref)^0.5),
#         v = irr)
# }




panel.3dwire <-
    function(x, y, z, rot.mat = diag(4), distance,
             col.at, col.regions,
             shade = FALSE,
             shade.colors = trellis.par.get("shade.colors")$palette,
             light.source = c(0, 0, 1000),
             xlim.scaled,
             ylim.scaled,
             zlim.scaled,
             col = "black",
             col.groups = superpose.line$col,
             polynum = 100,
             ...)
{
    ## a faster version of panel.3dwire that takes advantage of grid
    ## in R >= 1.8.1's multiple polygon drawing capabilities. The
    ## solution is a bit hackish, it basically keeps track of the
    ## polygons to be drawn in a 'global' variable and draws them all
    ## at once when 'sufficiently many' have been collected.

    ## x, y, z are in a special form here (compared to most other
    ## places in lattice). x and y are short ascending, describing the
    ## grid, and z is the corresponding z values in the order (x1,y1),
    ## (x1,y2), ... . length(z) == length(x) * length(y). Sometimes, z
    ## might be a matrix, which indicates multiple surfaces. Above
    ## description true for each column in that case.


    ## things are slightly different depending on whether shading is
    ## being done. If not, things are relatively simple, and in
    ## particular polygons are drawn one quadrilateral at a
    ## time. However, if shade=T, facets are drawn triangles at a
    ## time. The difference is mostly in C code, but some distinctions
    ## need to be made here as well












    ## 2004-03-12 new experimental stuff: when x, y, z are all
    ## matrices of the same dimension, they represent a 3-D surface
    ## parametrized on a 2-D grid (the details of the parametrizing
    ## grid are unimportant). 

    isParametrizedSurface <- is.matrix(x) && is.matrix(y) && is.matrix(z)

    if (isParametrizedSurface)
    {
        x[x < xlim.scaled[1] | x > xlim.scaled[2]] <- NA
        y[y < ylim.scaled[1] | y > ylim.scaled[2]] <- NA
        z[z < zlim.scaled[1] | z > zlim.scaled[2]] <- NA
        htrange <- extend.limits(sqrt(range(x^2 + y^2 + z^2, na.rm = TRUE)), prop = 0.01)
        ngroups <- 1
    }
    else
    {
        ngroups <- if (is.matrix(z)) ncol(z) else 1
        superpose.line <- trellis.par.get("superpose.line")
        col.groups <- rep(col.groups, length = ngroups)
        if (length(col) > 1) col <- rep(col, length = ngroups)


        ## remove things outside xlim and ylim bounds

        id.x <- x >= xlim.scaled[1] & x <= xlim.scaled[2]
        id.y <- y >= ylim.scaled[1] & y <= ylim.scaled[2]

        id.z <- rep(id.y, length(id.x)) & rep(id.x, each = length(id.y))

        x <- x[id.x]
        y <- y[id.y]
        z <- z[id.z]

        htrange <- zlim.scaled
    }


    if (shade)
    {

        shade.colors <-
            if (is.character(shade.colors)) get(shade.colors)
            else eval(shade.colors)

        pol.x <- numeric(polynum * 3)
        pol.y <- numeric(polynum * 3)

        if (shade) {
            pol.fill <- character(polynum)
            pol.col <- "transparent"
        }
        else if (length(col.regions) > 1) {
            pol.fill <- vector(mode(col.regions), polynum)
            pol.col <-
                if (ngroups == 1 || length(col) == 1) col[1]
                else vector(mode(col), polynum)
        }
        else if (ngroups == 1) {
            pol.fill <- col.regions[1]
            pol.col <- col[1]
        }
        else {
            pol.fill <- vector(mode(col.groups), polynum)
            pol.col <-
                if (length(col) == 1) col[1]
                else vector(mode(col), polynum)
        }


        count <- 0 ## counts number of polygons stacked up so far

        wirePolygon <-

            function(xx, yy, misc)
            {
                ## misc:
                ## 1: cos angle between normal and incident light
                ## 2: cos angle between reflected light and eye
                ## 3: z-height averaged
                ## 4: group indicator

                height <- (misc[3] - htrange[1]) / diff(htrange)
                invalid <- (is.na(height) || any(is.na(xx)) ||
                            any(is.na(yy)) || height > 1 || height < 0)

                if (!invalid)
                {
                    pol.x[3 * count + 1:3] <<- xx
                    pol.y[3 * count + 1:3] <<- yy

                    count <<- count + 1
                    pol.fill[count] <<- shade.colors(misc[1], misc[2], height)

                    if (count == polynum)
                    {
                        grid.polygon(x = pol.x, y = pol.y, id.length = rep(3, polynum),
                                     default.units = "native",
                                     gp = gpar(fill = pol.fill, col = pol.col))
                        count <<- 0
                    }
                }
            }


        .Call("wireframePanelCalculations",
              as.double(x),
              as.double(y),
              as.double(z),
              as.double(rot.mat),
              as.double(distance),
              if (isParametrizedSurface) as.integer(ncol(x)) else as.integer(length(x)),
              if (isParametrizedSurface) as.integer(nrow(x)) else as.integer(length(y)),
              as.integer(ngroups),
              as.double(light.source),
              environment(),
              as.integer(shade),
              as.integer(isParametrizedSurface),
              PACKAGE="lattice")


        if (count > 0)
        {
            grid.polygon(x = pol.x[1:(count * 3)], y = pol.y[1:(count * 3)],
                         default.units = "native", id.length = rep(3, count),
                         gp = gpar(fill = rep(pol.fill, length = count),
                         col = rep(pol.col, length = count)))
        }

    }
    else  ## no shade
    {

        pol.x <- numeric(polynum * 4)
        pol.y <- numeric(polynum * 4)

        if (length(col.regions) > 1)
        {
            pol.fill <- vector(mode(col.regions), polynum)
            pol.col <-
                if (ngroups == 1 || length(col) == 1) col[1]
                else vector(mode(col), polynum)
        }
        else if (ngroups == 1)
        {
            pol.fill <- col.regions[1]
            pol.col <- col[1]
        }
        else
        {
            pol.fill <- vector(mode(col.groups), polynum)
            pol.col <-
                if (length(col) == 1) col[1]
                else vector(mode(col), polynum)
        }


        count <- 0 ## counts number of polygons stacked up so far

        wirePolygon <-

            function(xx, yy, misc)
            {
                ## misc:
                ## 3: z-height averaged
                ## 4: group indicator

                height <- (misc[3] - htrange[1]) / diff(htrange)
                invalid <- (is.na(height) || any(is.na(xx)) || any(is.na(yy)) ||
                            height > 1 || height < 0)

                if (!invalid)
                {
                    pol.x[4 * count + 1:4] <<- xx
                    pol.y[4 * count + 1:4] <<- yy

                    count <<- count + 1

                    if (length(col.regions) > 1)
                    {
                        pol.fill[count] <<- col.regions[(seq(along = col.at)[col.at > misc[3]])[1] - 1 ]
                        if (ngroups > 1 && length(col) > 1) pol.col[count] <<- col[as.integer(misc[4])]
                    }
                    ## nothing to do if ngroups == 1
                    else if (ngroups > 1)
                    {
                        pol.fill[count] <<- col.groups[as.integer(as.integer(misc[4]))]
                        if (length(col) > 1) pol.col[count] <<- col[as.integer(misc[4])]
                    }


                    if (count == polynum) {

                        grid.polygon(x = pol.x, y = pol.y, id.length = rep(4, polynum),
                                     default.units = "native",
                                     gp = gpar(fill = pol.fill, col = pol.col))
                        count <<- 0
                    }
                }
            }



        .Call("wireframePanelCalculations",
              as.double(x),
              as.double(y),
              as.double(z),
              as.double(rot.mat),
              as.double(distance),
              if (isParametrizedSurface) as.integer(ncol(x)) else as.integer(length(x)),
              if (isParametrizedSurface) as.integer(nrow(x)) else as.integer(length(y)),
              as.integer(ngroups),
              as.double(light.source),
              environment(),
              as.integer(shade),
              as.integer(isParametrizedSurface),
              PACKAGE="lattice")



        if (count > 0)
        {
            grid.polygon(x = pol.x[1:(count * 4)], y = pol.y[1:(count * 4)],
                         default.units = "native", id.length = rep(4, count),
                         gp = gpar(fill = rep(pol.fill, length = count),
                         col = rep(pol.col, length = count)))
        }

    }

}













panel.cloud <-
    function(x, y, z, subscripts,
             groups = NULL,
             distance, xlim, ylim, zlim,
             panel.3d.cloud = "panel.3dscatter",
             panel.3d.wireframe = "panel.3dwire",
             rot.mat, aspect,
             par.box = NULL,

             xlab, ylab, zlab,
             xlab.default, ylab.default, zlab.default,

             scales.3d,
             proportion = 0.6, wireframe = FALSE,

             ## The main problem with scales is that it is difficult
             ## to figure out the best way to place the scales.  They
             ## be specified explicitly using scpos if default is not
             ## OK

             scpos,
             ...,
             col.at,
             col.regions)

{
    ## x, y, z can be matrices
    mode(x) <- "numeric"
    mode(y) <- "numeric"
    mode(z) <- "numeric"



    ## 2004-03-12 new experimental stuff: when x, y, z are all
    ## matrices of the same dimension, they represent a 3-D surface
    ## parametrized on a 2-D grid (the details of the parametrizing
    ## grid are unimportant). This is meant only for wireframe

    ## In this case, subscripts will be ignored, because it's not
    ## clear how they should interact

    isParametrizedSurface <-
        wireframe && is.matrix(x) && is.matrix(y) && is.matrix(z)

    if (isParametrizedSurface)
        zrng <- extend.limits(sqrt(range(x^2 + y^2 + z^2, na.rm = TRUE)))





    if (any(subscripts))  ## otherwise nothing to draw (not even box ?)
    {

        ## figure out data ranges and tick locations / labels
        ## Information needed: *lim, scales.3d

        ## For now, keep *lim as they are, since in cloud / wireframe
        ## extend.limits type adjustments don't happen by
        ## default. Even if that's done, this may not be the place to
        ## do it (shouldn't really need anything more than *lim and
        ## *axs)

        ## So, get tick labels, and then convert *lim to numeric
        ## Although, this is all unnecessary if arrows = TRUE


        xlabelinfo <-
            calculateAxisComponents(xlim,
                                    at = scales.3d$x$at,
                                    labels = scales.3d$x$labels,
                                    logsc = scales.3d$x$log,
                                    abbreviate = scales.3d$x$abbreviate,
                                    minlength = scales.3d$x$minlength,
                                    format.posixt = scales.3d$x$format,
                                    n = scales.3d$x$tick.number)


        ylabelinfo <-
            calculateAxisComponents(ylim,
                                    at = scales.3d$y$at,
                                    labels = scales.3d$y$labels,
                                    logsc = scales.3d$y$log,
                                    abbreviate = scales.3d$y$abbreviate,
                                    minlength = scales.3d$y$minlength,
                                    format.posixt = scales.3d$y$format,
                                    n = scales.3d$y$tick.number)


        zlabelinfo <-
            calculateAxisComponents(zlim,
                                    at = scales.3d$z$at,
                                    labels = scales.3d$z$labels,
                                    logsc = scales.3d$z$log,
                                    abbreviate = scales.3d$z$abbreviate,
                                    minlength = scales.3d$z$minlength,
                                    format.posixt = scales.3d$z$format,
                                    n = scales.3d$z$tick.number)

        x.at <- xlabelinfo$at
        y.at <- ylabelinfo$at
        z.at <- zlabelinfo$at

        x.at.lab <- xlabelinfo$lab
        y.at.lab <- ylabelinfo$lab
        z.at.lab <- zlabelinfo$lab

        xlim <- xlabelinfo$num.limit
        ylim <- ylabelinfo$num.limit
        zlim <- zlabelinfo$num.limit

        par.box.final <- trellis.par.get("box.3d")
        if (!is.null(par.box)) par.box.final[names(par.box)] <- par.box

        aspect <- rep(aspect, length=2)

        if (!isParametrizedSurface)
        {
            x <- x[subscripts]
            y <- y[subscripts]
            z <- z[subscripts]
        }

        corners <-
            data.frame(x = c(-1, 1, 1,-1,-1, 1, 1,-1),
                       y = c(-1,-1,-1,-1, 1, 1, 1, 1) * aspect[1],
                       z = c(-1,-1, 1, 1,-1,-1, 1, 1) * aspect[2])
        corners <- corners / (2 * max(corners)) ## contain in [-.5, .5] cube

        xlim.scaled <- range(corners$x)
        ylim.scaled <- range(corners$y)
        zlim.scaled <- range(corners$z)

        ## denotes scaled ranges of bounding box. passed to
        ## panel.3dscatter and panel.3dwire in case they are useful

        ## center of bounding box:
        box.center <- matrix(unlist(lapply(corners, mean)), 3, 1)

        ## these are box boundaries:
        pre <- c(1,2,4,1,2,3,4,1,5,6,8,5)
        nxt <- c(2,3,3,4,6,7,8,5,6,7,7,8)

        ## The corners are defined in terms of coordinates in 3-D
        ## space as above. The actual choice of coordinates ideally
        ## should not affect anything, but I haven't checked. Box
        ## boundaries are defined as pairs of corners. The numbers of
        ## the corners and boundaries are helpful in keeping track of
        ## things, and are described in the diagram below.


        ## 1, 2, ..., 8 are the corners, L-1, ..., L-12 the boundaries
        ##
        ##                                   L-11
        ##                           8------------------------7
        ##                         / |                       / |
        ##                        /  |                      /  |
        ##                    L-7/   |L-12              L-6/   |
        ##                      /    |                    /    |
        ##                     /     |                   /     |
        ##                    /      |        L-3       /      |L-10
        ##                   4-------------------------3       |
        ##                   |       |                 |       |
        ##                   |       |                 |       |
        ##                   |       |                 |       |
        ##                   |       |    L-9          |       |
        ##                L-4|       5-----------------|-------6
        ##                   |      /                  |      /
        ##                   |     /                   |     /
        ##                   |    /                 L-2|    /L-5
        ##                   |   /                     |   /
        ##                   |  /L-8                   |  /
        ##                   | /                       | /
        ##                   |/                        |/
        ##                   1-------------------------2
        ##                (0,0,0)          L-1
        ##
        ##
        ## Also the 6 FACES are defined in terms of corners (lines)
        ## as follows:
        ##
        ## F-1 : 1,2,3,4 (1,2,3,4)
        ## F-2 : 2,6,7,3 (5,10,6,2)
        ## F-3 : 6,5,8,7 (9,12,11,10)
        ## F-4 : 5,1,4,8 (8,4,7,12)
        ## F-5 : 1,2,6,5 (1,5,9,8)
        ## F-6 : 4,3,7,8 (3,6,11,7)

        face.corners <- list(c(1,2,3,4),
                             c(2,6,7,3),
                             c(6,5,8,7),
                             c(5,1,4,8),
                             c(1,2,6,5),
                             c(4,3,7,8))

        face.lines <- list(c(1,2,3,4),
                           c(5,10,6,2),
                           c(9,12,11,10),
                           c(8,4,7,12),
                           c(1,5,9,8),
                           c(3,6,11,7))

        ## SCALES : very beta

        tmp <- ltransform3dto3d(t(as.matrix(corners)), rot.mat)
        farthest <- 1  ## used later also
        farval <- tmp[3,1]

        for (i in 2:8)
            if (tmp[3,i] < farval) {
                farthest <- i
                farval <- tmp[3,i]
            }

        ## not foolproof, need to revisit this later
        scale.position <-
            if (farthest == 1) list(x = 3, y = 7, z = 2)
            else if (farthest == 2) list(x = 9, y = 8, z = 10)
            else if (farthest == 3) list(x = 11, y = 7, z = 10)
            else if (farthest == 4) list(x = 11, y = 6, z = 2)
            else if (farthest == 5) list(x = 1, y = 5, z = 4)
            else if (farthest == 6) list(x = 1, y = 8, z = 12)
            else if (farthest == 7) list(x = 3, y = 7, z = 2)
            else if (farthest == 8) list(x = 3, y = 6, z = 10)

        ##original:
            #if (farthest == 1) list(x = 9, y = 5, z = 2)
            #else if (farthest == 2) list(x = 9, y = 8, z = 10)
            #else if (farthest == 3) list(x = 11, y = 7, z = 10)
            #else if (farthest == 4) list(x = 11, y = 6, z = 2)
            #else if (farthest == 5) list(x = 1, y = 5, z = 4)
            #else if (farthest == 6) list(x = 1, y = 8, z = 12)
            #else if (farthest == 7) list(x = 3, y = 7, z = 2)
            #else if (farthest == 8) list(x = 3, y = 6, z = 10)

        if (!missing(scpos))
            scale.position[names(scpos)] <- scpos

        scpos <- scale.position


        labs <- rbind(x = c(0, corners$x[pre[scpos$y]], corners$x[pre[scpos$z]]),
                      y = c(corners$y[pre[scpos$x]], 0, corners$y[pre[scpos$z]]),
                      z = c(corners$z[pre[scpos$x]], corners$z[pre[scpos$y]], 0))

        labs[,1] <- labs[,1] * (1 + scales.3d$x.scales$distance/3)
        labs[,2] <- labs[,2] * (1 + scales.3d$y.scales$distance/3)
        labs[,3] <- labs[,3] * (1 + scales.3d$z.scales$distance/3)

        axes <- rbind(x =
                      c(proportion * corners$x[c(pre[scpos$x], nxt[scpos$x])],
                        corners$x[c(pre[scpos$y], nxt[scpos$y])],
                        corners$x[c(pre[scpos$z], nxt[scpos$z])]),
                      y =
                      c(corners$y[c(pre[scpos$x], nxt[scpos$x])],
                        proportion * corners$y[c(pre[scpos$y], nxt[scpos$y])],
                        corners$y[c(pre[scpos$z], nxt[scpos$z])]),
                      z =
                      c(corners$z[c(pre[scpos$x], nxt[scpos$x])],
                        corners$z[c(pre[scpos$y], nxt[scpos$y])],
                        proportion * corners$z[c(pre[scpos$z], nxt[scpos$z])]))

        axes[,1:2] <- axes[,1:2] * (1 + scales.3d$x.scales$distance/10)
        axes[,3:4] <- axes[,3:4] * (1 + scales.3d$y.scales$distance/10)
        axes[,5:6] <- axes[,5:6] * (1 + scales.3d$z.scales$distance/10)






        ## box ranges and lengths
        cmin <- lapply(corners, min)
        cmax <- lapply(corners, max)
        clen <- lapply(corners, function(x) diff(range(x)))


        ## scaled (to bounding box) data
        x <- cmin$x + clen$x * (x-xlim[1])/diff(xlim)
        y <- cmin$y + clen$y * (y-ylim[1])/diff(ylim)
        z <- cmin$z + clen$z * (z-zlim[1])/diff(zlim)
        col.at <-
            if (isParametrizedSurface)
            {
                zrng.scaled <- extend.limits(sqrt(range(x^2 + y^2 + z^2, na.rm = TRUE)))
                zrng.scaled[1] + diff(zrng.scaled) * (col.at - zrng[1])/diff(zrng)
            }
            else cmin$z + clen$z * (col.at - zlim[1])/diff(zlim)

        zero.scaled <- cmin$z - clen$z * zlim[1]/diff(zlim)
        ## needed in panel.3dscatter for type = 'h'


        x.at <- cmin$x + clen$x * (x.at-xlim[1])/diff(xlim)
        y.at <- cmin$y + clen$y * (y.at-ylim[1])/diff(ylim)
        z.at <- cmin$z + clen$z * (z.at-zlim[1])/diff(zlim)
        at.len <- length(x.at)
        x.at <- rbind(x = x.at,
                      y = rep(corners$y[pre[scpos$x]], at.len),
                      z = rep(corners$z[pre[scpos$x]], at.len))
        at.len <- length(y.at)
        y.at <- rbind(x = rep(corners$x[pre[scpos$y]], at.len),
                      y = y.at,
                      z = rep(corners$z[pre[scpos$y]], at.len))
        at.len <- length(z.at)
        z.at <- rbind(x = rep(corners$x[pre[scpos$z]], at.len),
                      y = rep(corners$y[pre[scpos$z]], at.len),
                      z = z.at)

        x.at.end <- x.at + scales.3d$x.scales$tck * .05 * labs[,1]
        y.at.end <- y.at + scales.3d$y.scales$tck * .05 * labs[,2]
        z.at.end <- z.at + scales.3d$z.scales$tck * .05 * labs[,3]

        x.labs <- x.at + 2 * scales.3d$x.scales$tck * .05 * labs[,1]
        y.labs <- y.at + 2 * scales.3d$y.scales$tck * .05 * labs[,2]
        z.labs <- z.at + 2 * scales.3d$z.scales$tck * .05 * labs[,3]


        corners <- ltransform3dto3d(t(as.matrix(corners)), rot.mat, distance)

        taxes <- ltransform3dto3d(axes, rot.mat, distance)
        x.at <- ltransform3dto3d(x.at, rot.mat, distance)
        x.labs <- ltransform3dto3d(x.labs, rot.mat, distance)
        x.at.end <- ltransform3dto3d(x.at.end, rot.mat, distance)

        y.at <- ltransform3dto3d(y.at, rot.mat, distance)
        y.labs <- ltransform3dto3d(y.labs, rot.mat, distance)
        y.at.end <- ltransform3dto3d(y.at.end, rot.mat, distance)

        z.at <- ltransform3dto3d(z.at, rot.mat, distance)
        z.labs <- ltransform3dto3d(z.labs, rot.mat, distance)
        z.at.end <- ltransform3dto3d(z.at.end, rot.mat, distance)

        tlabs <- ltransform3dto3d(labs, rot.mat, distance)

        box.center <- ltransform3dto3d(box.center, rot.mat, distance)

        ## Shall now determine which bounding lines should be 'hidden'
        ## (by the data, and hence need to be drawn before the data),
        ## and which should be 'visible'. Will actually consider each
        ## face (one at a time), determine if it is 'visible' (had the
        ## bounding cube been opaque), and if so, mark the lines
        ## forming that face as 'visible'

        ## The logical vector 'mark' will correspond to the 12 lines
        ## (indexing explained in the diagram above). mark = TRUE will
        ## mean that the line will be drawn AFTER the data is
        ## drawn. Start off with all mark = FALSE.

        ## The idea is that for visible faces, the z-value of the
        ## center of the face will be greater than the z-value of the
        ## center of the whole box. This doesn't always work for
        ## perspective plots.

        ##print(box.center)
        mark <- rep(FALSE, 12)
        box.center.z <- box.center[3]

        for (face in 1:6)
            if (mean(corners[3, face.corners[[face]] ]) > box.center.z) ## i.e., face visible
                mark[1:12 %in% face.lines[[face]] ] <- TRUE

        #for (j in 1:12)
        #    if (pre[j]==farthest || nxt[j]==farthest)
        #        mark[j] <- FALSE

        ## This draws the 'back' of the box, i.e., the portion that
        ## should be hidden by the data. This doesn't always work
        ## properly

        lsegments(corners[1, pre[!mark]],
                  corners[2, pre[!mark]],
                  corners[1, nxt[!mark]],
                  corners[2, nxt[!mark]],
                  col = par.box.final$col,
                  lwd = par.box.final$lwd,
                  lty = 2)


        ## The following portion of code is responsible for drawing
        ## the part of the plot driven by the data. The modus operandi
        ## will be different for cloud and wireframe, since they have
        ## essentially different purpose. For cloud, the data is
        ## unstructured, and x, y and z are all passed to the
        ## panel.3d.cloud function. For wireframe, on the other hand,
        ## x and y must form a regular grid, which sort(unique(<x|y>))
        ## is enough to describe (o.w., greater chances of memory
        ## problems). z would then have to be supplied in a very
        ## particular order. All this is fine, but a problem arises if
        ## we want to allow groups -- multiple surfaces. One option is
        ## to supply a matrix (nx * ny by no.of.groups) for z. This is
        ## OK, but it precludes the posibility of supplying x and y as
        ## only their unique values from the very beginning. So that's
        ## not allowed for grouped displays


        if (wireframe)
        {
            if (isParametrizedSurface)
            {
                ## FIXME: unnecessary copy
                tmp <- z
            }
            else if (is.null(groups))
            {
                nx <- length(unique(x))
                ny <- length(unique(y))
                len <- length(z)
                if (nx * ny == len)
                {
                    ord <- order(x, y)
                    tmp <- z[ord]
                    x <- sort(unique(x[!is.na(x)]))
                    y <- sort(unique(y[!is.na(y)]))
                }
                else
                {
                    ## which means some rows missing, should be NA

                    ## convert z into a (conceptual) matrix, with NA
                    ## entries for those 'missing' from data
                    ## frame. There's scope for ambiguity here, which
                    ## can be avoided by the user.

                    tmp <- rep(NA, nx * ny)
                    ux <- sort(unique(x[!is.na(x)]))
                    uy <- sort(unique(y[!is.na(y)]))
                    idx <- match(x, ux)
                    idy <- match(y, uy)
                    tmp[(idx - 1) * length(uy) + idy] <- z

                    x <- ux
                    y <- uy
                }
            }
            else {

                ## all surfaces have to be on the same regular
                ## grid. No row can be missing, though some z-values
                ## can be NA. Needs a lot of change otherwise

                vals <- sort(unique(groups))
                nvals <- length(vals)
                tmp <- numeric(0)

                for (i in seq(along=vals)) {
                    id <- (groups[subscripts] == vals[i])
                    if (any(id)) {
                        ord <- order(x[id], y[id])
                        tmp <- cbind(tmp, z[id][ord])
                    }
                }

                x <- sort(unique(x))
                y <- sort(unique(y))
            }

            z <- list(NULL) ## hopefully becomes garbage, collected if necessary

            panel.3d.wireframe <-
                if (is.character(panel.3d.wireframe)) get(panel.3d.wireframe)
                else eval(panel.3d.wireframe)

            pargs <- list(x = x, y = y, z = tmp,
                          rot.mat = rot.mat,
                          distance = distance,
                          col.at = col.at,
                          col.regions = col.regions,
                          xlim = xlim,
                          ylim = ylim,
                          zlim = zlim,
                          xlim.scaled = xlim.scaled,
                          ylim.scaled = ylim.scaled,
                          zlim.scaled = zlim.scaled,
                          zero.scaled = zero.scaled,
                          ...)


            if (!("..." %in% names(formals(panel.3d.wireframe))))
                pargs <- pargs[names(formals(panel.3d.wireframe))]
            do.call("panel.3d.wireframe", pargs)

        }
        else {

            panel.3d.cloud <-
                if (is.character(panel.3d.cloud)) get(panel.3d.cloud)
                else eval(panel.3d.cloud)

            pargs <- list(x = x, y = y, z = z,
                          rot.mat = rot.mat, distance,
                          groups = groups,
                          subscripts = subscripts,
                          xlim = xlim,
                          ylim = ylim,
                          zlim = zlim,
                          xlim.scaled = xlim.scaled,
                          ylim.scaled = ylim.scaled,
                          zlim.scaled = zlim.scaled,
                          zero.scaled = zero.scaled,
                          ...)

            if (!("..." %in% names(formals(panel.3d.cloud))))
                pargs <- pargs[names(formals(panel.3d.cloud))]
            do.call("panel.3d.cloud", pargs)
        }





        ## This draws the front of the bounding box

        lsegments(corners[1, pre[mark]],
                  corners[2, pre[mark]],
                  corners[1, nxt[mark]],
                  corners[2, nxt[mark]],
                  col = par.box.final$col,
                  lty = par.box.final$lty,
                  lwd = par.box.final$lwd)

        ## Next part for axes

        axis.text <- trellis.par.get("axis.text")
        axis.line <- trellis.par.get("axis.line")

        xaxis.col.line <-
            if (is.logical(scales.3d$x.scales$col.line)) axis.line$col
            else scales.3d$x.scales$col.line
        xaxis.lty <-
            if (is.logical(scales.3d$x.scales$lty)) axis.line$lwd
            else scales.3d$x.scales$lty
        xaxis.lwd <-
            if (is.logical(scales.3d$x.scales$lwd)) axis.line$lty
            else scales.3d$x.scales$lwd
        xaxis.col.text <-
            if (is.logical(scales.3d$x.scales$col)) axis.text$col
            else scales.3d$x.scales$col
        xaxis.font <-
            if (is.logical(scales.3d$x.scales$font)) axis.text$font
            else scales.3d$x.scales$font
        xaxis.fontface <-
            if (is.logical(scales.3d$x.scales$fontface)) axis.text$fontface
            else scales.3d$x.scales$fontface
        xaxis.fontfamily <-
            if (is.logical(scales.3d$x.scales$fontfamily)) axis.text$fontfamily
            else scales.3d$x.scales$fontfamily
        xaxis.cex <-
            if (is.logical(scales.3d$x.scales$cex)) rep(axis.text$cex, length = 1)
            else scales.3d$x.scales$cex
        xaxis.rot <-
            if (is.logical(scales.3d$x.scales$rot)) 0
            else scales.3d$x.scales$rot


        yaxis.col.line <-
            if (is.logical(scales.3d$y.scales$col.line)) axis.line$col
            else scales.3d$y.scales$col.line
        yaxis.lty <-
            if (is.logical(scales.3d$y.scales$lty)) axis.line$lwd
            else scales.3d$y.scales$lty
        yaxis.lwd <-
            if (is.logical(scales.3d$y.scales$lwd)) axis.line$lty
            else scales.3d$y.scales$lwd
        yaxis.col.text <-
            if (is.logical(scales.3d$y.scales$col)) axis.text$col
            else scales.3d$y.scales$col
        yaxis.font <-
            if (is.logical(scales.3d$y.scales$font)) axis.text$font
            else scales.3d$y.scales$font
        yaxis.fontface <-
            if (is.logical(scales.3d$y.scales$fontface)) axis.text$fontface
            else scales.3d$y.scales$fontface
        yaxis.fontfamily <-
            if (is.logical(scales.3d$y.scales$fontfamily)) axis.text$fontfamily
            else scales.3d$y.scales$fontfamily
        yaxis.cex <-
            if (is.logical(scales.3d$y.scales$cex)) rep(axis.text$cex, length = 1)
            else scales.3d$y.scales$cex
        yaxis.rot <-
            if (is.logical(scales.3d$y.scales$rot)) 0
            else scales.3d$y.scales$rot


        zaxis.col.line <-
            if (is.logical(scales.3d$z.scales$col.line)) axis.line$col
            else scales.3d$z.scales$col.line
        zaxis.lty <-
            if (is.logical(scales.3d$z.scales$lty)) axis.line$lwd
            else scales.3d$z.scales$lty
        zaxis.lwd <-
            if (is.logical(scales.3d$z.scales$lwd)) axis.line$lty
            else scales.3d$z.scales$lwd
        zaxis.col.text <-
            if (is.logical(scales.3d$z.scales$col)) axis.text$col
            else scales.3d$z.scales$col
        zaxis.font <-
            if (is.logical(scales.3d$z.scales$font)) axis.text$font
            else scales.3d$z.scales$font
        zaxis.fontface <-
            if (is.logical(scales.3d$z.scales$fontface)) axis.text$fontface
            else scales.3d$z.scales$fontface
        zaxis.fontfamily <-
            if (is.logical(scales.3d$z.scales$fontfamily)) axis.text$fontfamily
            else scales.3d$z.scales$fontfamily
        zaxis.cex <-
            if (is.logical(scales.3d$z.scales$cex)) rep(axis.text$cex, length = 1)
            else scales.3d$z.scales$cex
        zaxis.rot <-
            if (is.logical(scales.3d$z.scales$rot)) 0
            else scales.3d$z.scales$rot


        if (scales.3d$x.scales$draw) {
            if (scales.3d$x.scales$arrows) {
                larrows(x0 = taxes[1, 1], y0 = taxes[2, 1],
                        x1 = taxes[1, 2], y1 = taxes[2, 2],
                        lty = xaxis.lty,
                        lwd = xaxis.lwd,
                        col = xaxis.col.line)
            }
            else {
                lsegments(x0 = x.at[1,], y0 = x.at[2,], x1 = x.at.end[1,], y1 = x.at.end[2,],
                          lty = xaxis.lty,
                          col = xaxis.col.line,
                          lwd = xaxis.lwd)
                ltext(x.at.lab, x = x.labs[1,], y = x.labs[2,],
                      cex = xaxis.cex,
                      font = xaxis.font,
                      fontfamily = xaxis.fontfamily,
                      fontface = xaxis.fontface,
                      col = xaxis.col.text)
            }
        }

        if (scales.3d$y.scales$draw) {
            if (scales.3d$y.scales$arrows) {
                larrows(x0 = taxes[1, 3], y0 = taxes[2, 3],
                        x1 = taxes[1, 4], y1 = taxes[2, 4],
                        lty = yaxis.lty,
                        lwd = yaxis.lwd,
                        col = yaxis.col.line)
            }
            else {
                lsegments(x0 = y.at[1,], y0 = y.at[2,], x1 = y.at.end[1,], y1 = y.at.end[2,],
                          lty = yaxis.lty,
                          col = yaxis.col.line,
                          lwd = yaxis.lwd)
                ltext(y.at.lab, x = y.labs[1,], y = y.labs[2,],
                      cex = yaxis.cex,
                      font = yaxis.font,
                      fontfamily = yaxis.fontfamily,
                      fontface = yaxis.fontface,
                      col = yaxis.col.text)
            }
        }
        if (scales.3d$z.scales$draw) {
            if (scales.3d$z.scales$arrows) {
                larrows(x0 = taxes[1, 5], y0 = taxes[2, 5],
                        x1 = taxes[1, 6], y1 = taxes[2, 6],
                        lty = zaxis.lty,
                        lwd = zaxis.lwd,
                        col = zaxis.col.line)
            }
            else {
                lsegments(x0 = z.at[1,], y0 = z.at[2,], x1 = z.at.end[1,], y1 = z.at.end[2,],
                          lty = zaxis.lty,
                          col = zaxis.col.line,
                          lwd = zaxis.lwd)
                ltext(z.at.lab, x = z.labs[1,], y = z.labs[2,],
                      cex = zaxis.cex,
                      font = zaxis.font,
                      fontfamily = zaxis.fontfamily,
                      fontface = zaxis.fontface,
                      col = zaxis.col.text)
            }
        }



        xlab <- getLabelList(xlab, trellis.par.get("par.xlab.text"), xlab.default)
        ylab <- getLabelList(ylab, trellis.par.get("par.ylab.text"), ylab.default)
        zlab <- getLabelList(zlab, trellis.par.get("par.zlab.text"), zlab.default)

        if (!is.null(xlab))
            ltext(xlab$lab, x = tlabs[1, 1], y = tlabs[2, 1],
                  cex = xlab$cex,
                  rot = xlab$rot,
                  font = xlab$font,
                  fontfamily = xlab$fontfamily,
                  fontface = xlab$fontface,
                  col = xlab$col)
        if (!is.null(ylab))
            ltext(ylab$lab, x = tlabs[1, 2], y = tlabs[2, 2],
                  cex = ylab$cex, rot = ylab$rot,
                  font = ylab$font,
                  fontfamily = ylab$fontfamily,
                  fontface = ylab$fontface,
                  col = ylab$col)

        if (!is.null(zlab))
            ltext(zlab$lab, x = tlabs[1, 3], y = tlabs[2, 3],
                  cex = zlab$cex, rot = zlab$rot,
                  font = zlab$font,
                  fontfamily = zlab$fontfamily,
                  fontface = zlab$fontface,
                  col = zlab$col)
    }
}








panel.wireframe <- function(...)
    panel.cloud(..., wireframe = TRUE)





wireframe <-
    function(formula,
             data = parent.frame(),
             panel = "panel.wireframe",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 70,
             pretty = FALSE,
             drape = FALSE,
             ...,
             col.regions = trellis.par.get("regions")$col,
             colorkey = any(drape),
             subset = TRUE)
{
    ## m <- match.call(expand.dots = FALSE)
    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    do.call("cloud",
            c(list(formula = substitute(formula), data = data,
                   groups = groups, subset = subset,
                   panel = panel, prepanel = prepanel, strip = strip,
                   cuts = cuts,
                   pretty = pretty,
                   col.regions = col.regions,
                   drape = drape,
                   colorkey = colorkey,
                   axs.default = "i"),
              dots))
}























cloud <-
    function(formula,
             data = parent.frame(),
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = c(1,1),
             panel = "panel.cloud",
             prepanel = NULL,
             scales = NULL,
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim = if (is.factor(x)) levels(x) else range(x, na.rm = TRUE),
             ylab,
             ylim = if (is.factor(y)) levels(y) else range(y, na.rm = TRUE),
             zlab,
             zlim = if (is.factor(z)) levels(z) else range(z, na.rm = TRUE),
             distance = .2,
             perspective = TRUE,
             R.mat = diag(4),
             screen = list(z = 40, x = -60),
             zoom = .8,
             at,
             pretty = FALSE,
             drape = FALSE,
             drop.unused.levels = TRUE,
             ...,
             colorkey = any(drape),
             col.regions, cuts = 70,
             subset = TRUE,
             axs.default = "r")

    ## the axs.default is to (by default) enable scale extension for
    ## cloud, but not for wireframe. Needs work to be actually
    ## implemented.

{


    ##dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, z etc. and do some preprocessing

    left.name <- deparse(substitute(formula))
    formula <- eval(substitute(formula), data, parent.frame())
    form <-
        if (inherits(formula, "formula"))
            latticeParseFormula(formula, data, dim = 3,
                                subset = subset, groups = groups,
                                multiple = allow.multiple,
                                outer = outer, subscripts = TRUE,
                                drop = drop.unused.levels)
        else {
            if (is.matrix(formula)) {
                tmp <- expand.grid(1:nrow(formula), 1:ncol(formula))
                list(left = as.vector(formula),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     groups = groups,
                     left.name = left.name,
                     right.x.name = "row", right.y.name = "column",
                     subscr = seq(length = nrow(tmp)))
            }
            else if (is.data.frame(formula)) {
                tmp <- expand.grid(rownames(formula), colnames(formula))
                list(left = as.vector(as.matrix(formula)),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     groups = groups,
                     left.name = "left.name",
                     right.x.name = "row", right.y.name = "column",
                     subscr = seq(length = nrow(tmp)))
            }
            else stop("invalid formula")
        }

    ## We need to be careful with subscripts here. It HAS to be there,
    ## and it's to be used to index x, y, z (and not only groups,
    ## unlike in xyplot etc). This means we have to subset groups as
    ## well, which is about the only use for the subscripts calculated
    ## in latticeParseFormula, after which subscripts is regenerated
    ## as a straight sequence indexing the variables

    if (!is.null(form$groups))
        groups <-
            if (is.matrix(form$groups)) as.vector(form$groups)[form$subscr]
            else if (is.data.frame(form$groups)) as.vector(as.matrix(form$groups))[form$subscr]
            else form$groups[form$subscr]

    subscr <- seq(length = length(form$left))

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)


    cond <- form$condition
    number.of.cond <- length(cond)
    z <- form$left
    x <- form$right.x
    y <- form$right.y




    ## 2004-03-12 new experimental stuff: when x, y, z are all
    ## matrices of the same dimension, they represent a 3-D surface
    ## parametrized on a 2-D grid (the details of the parametrizing
    ## grid are unimportant). This is meant only for wireframe

    isParametrizedSurface <-
        is.matrix(x) && is.matrix(y) && is.matrix(z)





    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        number.of.cond <- 1
    }

    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name
    if (missing(zlab)) zlab <- form$left.name

    zrng <-
        if (isParametrizedSurface)
            extend.limits(sqrt(range(x^2 + y^2 + z^2, na.rm = TRUE)))
        else
            extend.limits(range(as.numeric(z), na.rm = TRUE))


    if (missing(at))
        at <-
            if (drape)
            {
                if (pretty) pretty(zrng, cuts)
                else seq(zrng[1], zrng[2], length = cuts+2)
            }
            else zrng

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(cond = cond,
                          aspect = 1,
                          strip = strip,
                          panel = panel,
                          xlab = NULL,
                          ylab = NULL), dots))

    ##----------------------------------------------------------------+
    ## xlab, ylab, zlab have special meaning in cloud / wireframe, and|
    ## need to be passed to the panel function to be processed. These |
    ## xlab / ylab are dummies to satisfy the usual processing        |
    ## routines ------------------------------------------------------+


    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## Step 2: Compute scales.common (leaving out limits for now)

    foo <- c(foo,
             do.call("construct.scales", list(draw=FALSE)))

    ## scales has to be interpreted differently. Nothing needs to be
    ## done for the ususal scales, but need a scales for panel.cloud
    ## S-PLUS probably doesn't allow x-y-z-specific scales, but I see
    ## no reason not to allow that (will not allow limits, though)


    scales.default <- list(distance = c(1, 1, 1), arrows = TRUE, axs = axs.default)
    if (!is.null(scales)) scales.default[names(scales)] <- scales
    scales.3d <- do.call("construct.3d.scales", scales.default)




    ## Step 3: Decide if limits were specified in call
    ## Here, always FALSE (in the 2d panel sense)
    have.xlim <- FALSE
    have.ylim <- FALSE

    ## Step 4: Decide if log scales are being used: !!!

    have.xlog <- !is.logical(scales.3d$x.scales$log) || scales.3d$x.scales$log
    have.ylog <- !is.logical(scales.3d$y.scales$log) || scales.3d$y.scales$log
    have.zlog <- !is.logical(scales.3d$z.scales$log) || scales.3d$z.scales$log
    if (have.xlog) {
        xlog <- scales.3d$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (!missing(xlim)) xlim <- log(xlim, xbase)
    }
    if (have.ylog) {
        ylog <- scales.3d$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (!missing(ylim)) ylim <- log(ylim, ybase)
    }
    if (have.zlog) {
        zlog <- scales.3d$z.scales$log
        zbase <-
            if (is.logical(zlog)) 10
            else if (is.numeric(zlog)) zlog
            else if (zlog == "e") exp(1)

        z <- log(z, zbase)
        if (!missing(zlim)) zlim <- log(zlim, zbase)
    }

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))


    ## don't really want to leave out NA's in z
    id.na <- is.na(x) | is.na(y)  ##|is.na(z)

    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")

    ## Nothing simpler ?


    ## Step 6: Evaluate layout, panel.args.common and panel.args


    ## calculate rotation matrix:


    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)

    if (!drape) col.regions <- trellis.par.get("background")$col

    ## region
    numcol <- length(at) - 1
    numcol.r <- length(col.regions)

    col.regions <-
        if (numcol.r <= numcol)
            rep(col.regions, length = numcol)
        else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]


    if (is.logical(colorkey))
    {
        if (colorkey) colorkey <-
            list(space = "right", col = col.regions,
                 at = at, tick.number = 7)
        else colorkey <- NULL
    }
    else if (is.list(colorkey))
    {
        ##foo$colorkey <- colorkey
        if (is.null(colorkey$col)) colorkey$col <- col.regions
        if (is.null(colorkey$at)) colorkey$at <- at
        if (is.null(colorkey$space)) colorkey$space <-
            if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right"
    }

    foo$legend <-
        construct.legend(foo$legend,
                         colorkey,
                         fun = "draw.colorkey")



    ## maybe *lim = NULL with relation = "free" ?


    ## Process limits here ? Needs some thought


    foo$panel.args.common <-
        c(list(x = x, y = y, z = z, rot.mat = rot.mat, zoom = zoom,
               xlim = xlim, ylim = ylim, zlim = zlim,
               xlab = xlab, ylab = ylab, zlab = zlab,
               xlab.default = form$right.x.name,
               ylab.default = form$right.y.name,
               zlab.default = form$left.name,
               aspect = aspect,
               distance = if (perspective) distance else 0,
               scales.3d = scales.3d,
               col.at = at, col.regions = col.regions),
          dots)

    if (!is.null(groups)) foo$panel.args.common$groups <- groups


    nplots <- prod(cond.max.level)
    if (nplots != prod(sapply(foo$condlevels, length))) stop("mismatch")
    foo$panel.args <- vector(mode = "list", length = nplots)


    cond.current.level <- rep(1, number.of.cond)


    for (panel.number in seq(length = nplots))
    {

        id <- !id.na
        for(i in 1:number.of.cond)
        {
            var <- cond[[i]]
            id <- id &
            if (is.shingle(var))
                ((var >=
                  levels(var)[[cond.current.level[i]]][1])
                 & (var <=
                    levels(var)[[cond.current.level[i]]][2]))
            else (as.numeric(var) == cond.current.level[i])
        }

        foo$panel.args[[panel.number]] <-
            list(subscripts = subscr[id])

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)

    }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.cloud,
                               prepanel = prepanel,
                               have.xlim = have.xlim, xlim = xlim,
                               have.ylim = have.ylim, ylim = ylim,
                               x.relation = foo$x.scales$relation,
                               y.relation = foo$y.scales$relation,
                               panel.args.common = foo$panel.args.common,
                               panel.args = foo$panel.args,
                               aspect = 1,
                               nplots = nplots))


    if (is.null(foo$legend) && !is.null(groups) &&
        (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                      args =
                      c(list(levels(as.factor(groups))),
                        if (is.list(auto.key)) auto.key else list())))
        foo$legend[[1]]$x <- foo$legend[[1]]$args$x
        foo$legend[[1]]$y <- foo$legend[[1]]$args$y
        foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

        names(foo$legend) <-
            if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
                "inside"
            else
                "top"
        if (!is.null(foo$legend[[1]]$args$space))
            names(foo$legend) <- foo$legend[[1]]$args$space
    }

    class(foo) <- "trellis"
    foo
}








