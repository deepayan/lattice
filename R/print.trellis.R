

### Copyright 2001  Deepayan Sarkar <deepayan@stat.wisc.edu>
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



## accessors for a grid layouts nrow and ncol

layoutNRow <- function(x) x$nrow
layoutNCol <- function(x) x$ncol






## the pos'th entry in the unit vector x is replaced by the unit u.
## Essentially does what x[pos] <- u should have done, only u can only
## be a unit of length 1



rearrangeUnit <- function(x, pos, u)
{
    if (unit.length(x) == 1)
        u
    else if (pos == 1)
        unit.c(u, x[(pos+1):unit.length(x)])
    else if (pos == unit.length(x))
        unit.c(x[1:(pos-1)], u)
    else
        unit.c(x[1:(pos-1)], u, x[(pos+1):unit.length(x)])
}







## utility to create a full-fledged list describing a label from parts
## (used for main, sub, xlab, ylab)

getLabelList <- function(label, text.settings, default.label = NULL)
{
    if (!is.null(label))
    {
        ans <- list(label = 
                    if (is.characterOrExpression(label)) label
                    else if (is.list(label) && names(label)[1] == "") label[[1]]
                    else default.label,
                    col = text.settings$col, cex = text.settings$cex,
                    fontfamily = text.settings$fontfamily,
                    fontface = text.settings$fontface,
                    font = text.settings$font)

        if (is.list(label)) ans[names(label)] <- label
    }
    else ans <- NULL
    if (is.null(ans$lab) ||
        (is.character(ans) && ans$lab == "")) ans <- NULL
    ans
}










# convenience function for auto.key

drawSimpleKey <- function(...)
    draw.key(simpleKey(...), draw = FALSE)





# convenience function for the most common type of key

simpleKey <- function(text, points = TRUE,
                      rectangles = FALSE,
                      lines = FALSE,
                      col = add.text$col,
                      cex = add.text$cex,
                      font = add.text$font,
                      fontface = add.text$fontface,
                      fontfamily = add.text$fontfamily,
                      ...)
{
    add.text <- trellis.par.get("add.text")
    foo <- seq(along = text)
    ans <- list(text = list(lab = text),
                col = col, cex = cex, font = font,
                fontface = fontface,
                fontfamily = fontfamily,
                ...)

    if (points) ans$points <-
        Rows(trellis.par.get("superpose.symbol"), foo)

    if (rectangles) {
        col.regions <- trellis.par.get("regions")$col
        numcol.r <- length(col.regions)
        numcol <- length(foo)
        ans$rectangles <-
            list(col = 
                 if (numcol.r <= numcol) rep(col.regions, length = numcol)
                 else col.regions[floor(1+(foo-1)*(numcol.r-1)/(numcol-1))])
    }
    if (lines) ans$lines <-
        Rows(trellis.par.get("superpose.line"), foo)
    ans
}
             





draw.key <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    
    max.length <- 0

    ## maximum of the `row-lengths' of the above
    ## components. There is some scope for confusion
    ## here, e.g., if col is specified in key as a
    ## length 6 vector, and then lines=list(lty=1:3),
    ## what should be the length of that lines column ?
    ## If 3, what happens if lines=list() ?
    ## (Strangely enough, S+ accepts lines=list()
    ## if col (etc) is NOT specified outside, but not
    ## if it is)
    
    process.key <-
        function(between = 2,
                 align = TRUE,
                 title = NULL,
                 rep = TRUE,
                 background = trellis.par.get("background")$col,
                 border = FALSE,
                 transparent = FALSE, 
                 columns = 1,
                 divide = 3,
                 between.columns = 3,
                 cex = 1,
                 cex.title = 1.5 * max(cex),
                 col = "black", 
                 lty = 1,
                 lwd = 1,
                 font = 1, 
                 fontface = NULL, 
                 fontfamily = NULL, 
                 pch = 8,
                 adj = 0,
                 type = "l", 
                 size = 5, 
                 angle = 0, 
                 density = -1,
                 ...)
        {
            list(between = between,
                 align = align,
                 title = title,
                 rep = rep,
                 background = background,
                 border = border,
                 transparent = transparent, 
                 columns = columns,
                 divide = divide,
                 between.columns = between.columns,
                 cex = cex,
                 cex.title = cex.title,
                 col = col,
                 lty = lty,
                 lwd = lwd,
                 font = font,
                 fontface = fontface,
                 fontfamily = fontfamily,
                 pch = pch,
                 adj = adj,
                 type = type, 
                 size = size, 
                 angle = angle, 
                 density = density,
                 ...)
        }

    fontsize.points <- trellis.par.get("fontsize")$points
    key <- do.call("process.key", key)

    key.length <- length(key)
    key.names <- names(key)    # Need to update
    if (is.logical(key$border)) 
        key$border <-
            if (key$border) "black"
            else "transparent"

    components <- list()

    for(i in 1:key.length) {

        curname <- pmatch(key.names[i], c("text", "rectangles", "lines", "points"))

        if (is.na(curname)) {
            ;## do nothing
        }
        else if (curname == 1) { # "text"
            if (!(is.characterOrExpression(key[[i]][[1]])))
                stop("first component of text has to be vector of labels")
            pars <- list(labels = key[[i]][[1]],
                         col = key$col,
                         adj = key$adj,
                         cex = key$cex,
                         font = key$font,
                         fontface = key$fontface,
                         fontfamily = key$fontfamily)
            key[[i]][[1]] <- NULL
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- length(pars$labels)
            for (j in 1:length(pars))
                if (is.character(pars))
                    pars[[j]] <- rep(pars[[j]], length = tmplen)

            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "text", pars = pars, length = tmplen)

        }
        else if (curname == 2) { # "rectangles"

            pars <- list(col = key$col,
                         size = key$size,
                         angle = key$angle,
                         density = key$density)
            
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "rectangles", pars = pars, length = tmplen)
            
        }
        else if (curname == 3) { # "lines"

            pars <- list(col = key$col,
                         size = key$size,
                         lty = key$lty,
                         cex = key$cex,
                         lwd = key$lwd,
                         type = key$type)

            pars[names(key[[i]])] <- key[[i]]

            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "lines", pars = pars, length = tmplen)
            
        }
        else if (curname == 4) { # "points"

            pars <- list(col = key$col,
                         cex = key$cex,
                         pch = key$pch,
                         font = key$font,
                         fontface = key$fontface,
                         fontfamily = key$fontfamily)
                         
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "points", pars = pars, length = tmplen)

        }
    }


    
    number.of.components <- length(components)
    ## number of components named one of "text",
    ## "lines", "rectangles" or "points"
    if (number.of.components == 0)
        stop("Invalid key, need at least one component named lines, text, rect or points")

    ## The next part makes sure all components have same length,
    ## except text, which should be as long as the number of labels

    ## Update (9/11/2003): but that doesn't always make sense --- Re:
    ## r-help message from Alexander.Herr@csiro.au (though it seems
    ## that's S+ behaviour on Linux at least). Each component should
    ## be allowed to have its own length (that's what the lattice docs
    ## suggest too, don't know why). Anyway, I'm adding a rep = TRUE
    ## argument to the key list, which controls whether each column
    ## will be repeated as necessary to have the same length.

    
    for (i in 1:number.of.components)
        if (components[[i]]$type != "text") {
            components[[i]]$pars <-
                lapply(components[[i]]$pars, rep,
                       length = if (key$rep) max.length
                       else components[[i]]$length)
            if (key$rep) components[[i]]$length <- max.length
        }
        else{
            ## NB: rep doesn't work with expressions of length > 1
            components[[i]]$pars <-
                c(components[[i]]$pars[1],
                  lapply(components[[i]]$pars[-1], rep,
                         length = components[[i]]$length))
        }

    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)

    if (column.blocks > max.length) warning("not enough rows for columns")
    
    key$between <- rep(key$between, length = number.of.components)

    
    if (key$align) {

        ## Setting up the layout


	## The problem of allocating space for text (character strings
	## or expressions) is dealt with as follows: 

	## Each row and column will take exactly as much space as
	## necessary. As steps in the construction, a matrix
	## textMatrix (of same dimensions as the layout) will contain
	## either 0, meaning that entry is not text, or n > 0, meaning
	## that entry has the text given by textList[[n]], where
	## textList is a list consisting of character strings or
	## expressions.



        n.row <- rows.per.block + 1
        n.col <- column.blocks * (1 + 3 * number.of.components) - 1

	textMatrix <- matrix(0, n.row, n.col)
	textList <- list()
	textCex <- numeric(0)

        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- as.list(1:n.row)

        if (length(key$title) > 0)
        {
            stopifnot(length(key$title) == 1,
                      is.characterOrExpression(key$title))
            heights.x[1] <- 1.2 * key$cex.title
            heights.units[1] <- "strheight"
            heights.data[[1]] <- key$title
        }
        else heights.x[1] <- 0


        widths.x <- rep(key$between.column, n.col)
        widths.units <- rep("strwidth", n.col)
        widths.data <- as.list(rep("o", n.col))



        for (i in 1:column.blocks) {
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i-1] <-
                         key$between/2
            
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i+1] <-
                         key$between/2
        }
    
        
	index <- 1

        for (i in 1:number.of.components) {

            cur <- components[[i]]

            id <- (1:column.blocks - 1) *
                (number.of.components * 3 + 1) + i * 3 - 1

            if (cur$type == "text") {

                for (j in 1:cur$length) {

                    colblck <- ceiling(j / rows.per.block)

                    xx <- (colblck - 1) *
                        (number.of.components * 3 + 1) + i * 3 - 1

                    yy <- j %% rows.per.block + 1
                    if (yy == 1) yy <- rows.per.block + 1

		    textMatrix[yy, xx] <- index
		    textList <- c(textList, list(cur$pars$labels[j]) )
		    textCex <- c(textCex, cur$pars$cex[j])
  		    index <- index + 1

		}


            } ## FIXME: do the same as above for those below
            else if (cur$type == "rectangles") {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "lines") {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "points") {
                widths.x[id] <- max(cur$pars$cex)
            }
        }


        ## Need to adjust the heights and widths 
        
        ## adjusting heights
        heights.insertlist.position <- 0
        heights.insertlist.unit <- unit(1, "null")

        for (i in 1:n.row) {
            textLocations <- textMatrix[i,]
            textLocations <- textLocations[textLocations>0]
            if (any(textLocations)) {

                strbar <- textList[textLocations]
                heights.insertlist.position <- c(heights.insertlist.position, i)
                heights.insertlist.unit <-
                    unit.c(heights.insertlist.unit,
                           unit(.2, "lines") + max(unit(textCex[textLocations], "strheight", strbar)))
            }
        }


        layout.heights <- unit(heights.x, heights.units, data=heights.data)
        if (length(heights.insertlist.position)>1)
            for (indx in 2:length(heights.insertlist.position))
                layout.heights <-
                    rearrangeUnit(layout.heights, heights.insertlist.position[indx],
                                  heights.insertlist.unit[indx])





        ## adjusting widths
        widths.insertlist.position <- 0
        widths.insertlist.unit <- unit(1, "null")




        for (i in 1:n.col) {
            textLocations <- textMatrix[,i]
            textLocations <- textLocations[textLocations>0]
            if (any(textLocations)) {

                strbar <- textList[textLocations]
                widths.insertlist.position <- c(widths.insertlist.position, i)
                widths.insertlist.unit <-
                    unit.c(widths.insertlist.unit,
                           max(unit(textCex[textLocations], "strwidth", strbar)))
            }
        }


        layout.widths <- unit(widths.x, widths.units, data=widths.data)
        if (length(widths.insertlist.position)>1)
            for (indx in 2:length(widths.insertlist.position))
                layout.widths <-
                    rearrangeUnit(layout.widths, widths.insertlist.position[indx],
                                  widths.insertlist.unit[indx])


        key.layout <- grid.layout(nrow = n.row, ncol = n.col,
                                  widths = layout.widths,
                                  heights = layout.heights,
                                  respect = FALSE)

        ## OK, layout set up, now to draw the key - no

        
        key.gf <- frameGrob(layout = key.layout, vp = vp)

        if (!key$transparent) 
            key.gf <- placeGrob(key.gf,
                                rectGrob(gp = gpar(fill = key$background, col = key$border)),
                                row = NULL, col = NULL)
        else
            key.gf <- placeGrob(key.gf,
                                rectGrob(gp=gpar(col=key$border)),
                                row = NULL, col = NULL)

        ## Title
        if (!is.null(key$title))
            key.gf <- placeGrob(key.gf, 
                                textGrob(label = key$title, gp = gpar(cex = key$cex.title)),
                                row=1, col = NULL)
        

        
        for (i in 1:number.of.components) {

            cur <- components[[i]]

            for (j in 1:cur$length) {

                colblck <- ceiling(j / rows.per.block)

                xx <- (colblck - 1) *
                    (number.of.components*3 + 1) + i*3 - 1

                yy <- j %% rows.per.block + 1
                if (yy == 1) yy <- rows.per.block + 1

                if (cur$type == "text") {
                    
                    key.gf <- placeGrob(key.gf, 
                                        textGrob(x = cur$pars$adj[j],
                                                 just = c(
                                                 if (cur$pars$adj[j] == 1) "right"
                                                 else if (cur$pars$adj[j] == 0) "left"
                                                 else "center",
                                                 "center"),
                                                 label = cur$pars$labels[j],
                                                 gp =
                                                 gpar(col = cur$pars$col[j],
                                                      fontfamily = cur$pars$fontfamily[j],
                                                      fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                      cex = cur$pars$cex[j])),
                                        row = yy, col = xx)
                    
                }
                else if (cur$type == "rectangles") {
                    key.gf <- placeGrob(key.gf, 
                              rectGrob(width = cur$pars$size[j]/max(cur$pars$size),
                                       ## centred, unlike Trellis, due to aesthetic reasons !
                                       gp = gpar(fill = cur$pars$col[j])),
                              row = yy, col = xx)

                    ## Need to make changes to support angle/density
                }
                else if (cur$type == "lines") {
                    if (cur$pars$type[j] == "l") {
                        key.gf <-
                            placeGrob(key.gf,
                                      linesGrob(x = c(0,1) * cur$pars$size[j]/max(cur$pars$size),

                                                ## ^^ this should be
                                                ## centered as well,
                                                ## but since the
                                                ## chances that
                                                ## someone would
                                                ## actually use this
                                                ## feature are
                                                ## astronomical, I'm
                                                ## leaving that for
                                                ## later.

                                                y = c(.5, .5),
                                                gp = gpar(col = cur$pars$col[j],
                                                lty = cur$pars$lty[j],
                                                lwd = cur$pars$lwd[j])),
                                  row = yy, col = xx)
                    }
                    else if (cur$pars$type[j] == "p") {
                        key.gf <-
                            placeGrob(key.gf,
                                      pointsGrob(x=.5, y=.5, 
                                                 gp =
                                                 gpar(col = cur$pars$col[j], cex = cur$pars$cex[j],
                                                      fontfamily = cur$pars$fontfamily[j],
                                                      fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                      fontsize = fontsize.points),
                                                 pch = cur$pars$pch[j]),
                                      row = yy, col = xx)
                    }
                    else { # if (cur$pars$type[j] == "b" or "o") -- not differentiating
                        key.gf <-
                            placeGrob(key.gf, 
                                      linesGrob(x = c(0,1) * cur$pars$size[j]/max(cur$pars$size),

                                                ## ^^ this should be
                                                ## centered as well,
                                                ## but since the
                                                ## chances that
                                                ## someone would
                                                ## actually use this
                                                ## feature are
                                                ## astronomical, I'm
                                                ## leaving that for
                                                ## later.

                                                y = c(.5, .5),
                                                gp = gpar(col = cur$pars$col[j],
                                                lty = cur$pars$lty[j],
                                                lwd = cur$pars$lwd[j])),
                                      row = yy, col = xx)

                        key.gf <-
                            placeGrob(key.gf, 
                                      pointsGrob(x = (1:key$divide-1)/(key$divide-1),
                                                 y = rep(.5, key$divide),
                                                 gp =
                                                 gpar(col = cur$pars$col[j], cex = cur$pars$cex[j],
                                                      fontfamily = cur$pars$fontfamily[j],
                                                      fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                      fontsize = fontsize.points),
                                                 pch = cur$pars$pch[j]),
                                      row = yy, col = xx)
                    }
                }
                else if (cur$type == "points") {
                    key.gf <- placeGrob(key.gf,
                                        pointsGrob(x=.5, y=.5,
                                                   gp =
                                                   gpar(col = cur$pars$col[j], cex = cur$pars$cex[j],
                                                        fontfamily = cur$pars$fontfamily[j],
                                                        fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                        fontsize = fontsize.points),
                                                   pch = cur$pars$pch[j]),
                                        row = yy, col = xx)
                }

            }

        }

    }
    else stop("sorry, align=F not supported (yet ?)")


    if (draw)
        grid.draw(key.gf)

    key.gf
}









draw.colorkey <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    
    process.key <-
        function(col,
                 at,
                 tick.number = 7,
                 width = 2,
                 height = 1,
                 space = "right",
                 ...)
        {
            list(col = col,
                 at = at,
                 tick.number = tick.number,
                 width = width,
                 height = height,
                 space = space,
                 ...)
        }

    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")

    key <- do.call("process.key", key)

    ## made FALSE later if labels explicitly specified
    check.overlap <- TRUE
    
    ## Getting the locations/dimensions/centers of the rectangles
    key$at <- sort(key$at) ## should check if ordered
    if (length(key$at)!=length(key$col)+1) stop("length(col) must be length(at)-1")

    atrange <- range(key$at)
    scat <- key$at

    recnum <- length(scat)-1
    reccentre <- (scat[-1] + scat[-length(scat)]) / 2
    recdim <- diff(scat)

    cex <- axis.text$cex
    col <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface <- axis.text$fontface

    if (is.null(key$lab))
    {
        at <- lpretty(atrange, key$tick.number)
        at <- at[at>=atrange[1] & at<=atrange[2]]
        labels <- format(at, trim = TRUE)
    }
    else if (is.characterOrExpression(key$lab) && length(key$lab)==length(key$at))
    {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    }
    else if (is.list(key$lab))
    {
        at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
        at <- at[at>=atrange[1] & at<=atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
        } else format(at, trim = TRUE)
        if (!is.null(key$lab$cex)) cex <- key$lab$cex
        if (!is.null(key$lab$col)) col <- key$lab$col
        if (!is.null(key$lab$font)) font <- key$lab$font
        if (!is.null(key$lab$fontface)) fontface <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) fontfamily <- key$lab$fontfamily
    }
    else stop("malformed colorkey")

    labscat <- at


    if (key$space == "right") {

        labelsGrob <-
            textGrob(label = labels,
                     x = rep(0, length(labscat)),
                     y = labscat,
                     vp = viewport(yscale = atrange),
                     default.units = "native",
                     check.overlap = check.overlap,
                     just = c("left","center"),
                     gp =
                     gpar(col = col,
                          cex = cex,
                          fontfamily = fontfamily,
                          fontface = chooseFace(fontface, font)))

        heights.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        heights.units <- rep("null", 3)

        widths.x <- c(.6 * key$width, .6, 1)
        widths.units <- c("lines", "lines", "grobwidth")
        widths.data <- list(NULL, NULL, labelsGrob)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units),
                        widths = unit(widths.x, widths.units, data = widths.data),
                        respect = TRUE)

        key.gf <- frameGrob(layout = key.layout, vp = vp)

        key.gf <- placeGrob(key.gf,
                            rectGrob(x = rep(.5, length(reccentre)), 
                                     y = reccentre,
                                     default.units = "native",
                                     vp = viewport(yscale = atrange),
                                     height = recdim, 
                                     gp = gpar(fill = key$col,  col = NULL)),
                            row = 2, col = 1)
        
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd)),
                            row = 2, col = 1)


        key.gf <- placeGrob(frame = key.gf, 
                            segmentsGrob(x0 = rep(0, length(labscat)),
                                         y0 = labscat,
                                         x1 = rep(.4, length(labscat)),
                                         y1 = labscat,
                                         vp = viewport(yscale = atrange),
                                         default.units = "native",
                                         gp =
                                         gpar(col = axis.line$col,
                                              lty = axis.line$lty,
                                              lwd = axis.line$lwd)),
                            row = 2, col = 2)
        
        key.gf <- placeGrob(key.gf,
                            labelsGrob, 
                            row = 2, col = 3)
#        key.gf <- placeGrob(key.gf,
#                            rectGrob(), 
#                            row = 2, col = 3)

    }
    else if (key$space == "left") {



        labelsGrob <-
            textGrob(label = labels,
                     x = rep(1, length(labscat)),
                     y = labscat,
                     vp = viewport(yscale = atrange),
                     default.units = "native",
                     check.overlap = check.overlap,
                     just = c("right","center"),
                     gp =
                     gpar(col = col,
                          cex = cex,
                          fontfamily = fontfamily,
                          fontface = chooseFace(fontface, font)))

        heights.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        heights.units <- rep("null", 3)

        widths.x <- c(1, .6, .6 * key$width)
        widths.units <- c("grobwidth", "lines", "lines")
        widths.data <- list(labelsGrob, NULL, NULL)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units),
                        widths = unit(widths.x, widths.units, data = widths.data),
                        respect = TRUE)
        
        key.gf <- frameGrob(layout = key.layout, vp = vp)

        key.gf <- placeGrob(key.gf,
                            rectGrob(x = rep(.5, length(reccentre)), 
                                     y = reccentre,
                                     default.units = "native",
                                     vp = viewport(yscale = atrange),
                                     height = recdim, 
                                     gp = gpar(fill = key$col,  col = NULL)),
                            row = 2, col = 3)
        
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd)),
                            row = 2, col = 3)


        key.gf <- placeGrob(frame = key.gf, 
                            segmentsGrob(x0 = rep(1, length(labscat)),
                                         y0 = labscat,
                                         x1 = rep(.6, length(labscat)),
                                         y1 = labscat,
                                         vp = viewport(yscale = atrange),
                                         default.units = "native",
                                         gp =
                                         gpar(col = axis.line$col,
                                              lty = axis.line$lty,
                                              lwd = axis.line$lwd)),
                            row = 2, col = 2)
        
        key.gf <- placeGrob(key.gf,
                            labelsGrob, 
                            row = 2, col = 1)

    }
    else if (key$space == "top") {

        labelsGrob <-
            textGrob(label = labels,
                     y = rep(0, length(labscat)),
                     x = labscat,
                     vp = viewport(xscale = atrange),
                     default.units = "native",
                     check.overlap = check.overlap,
                     just = c("center","bottom"),
                     gp =
                     gpar(col = col,
                          cex = cex,
                          fontfamily = fontfamily,
                          fontface = chooseFace(fontface, font)))

        widths.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        widths.units <- rep("null", 3)

        heights.x <- c(1, .6, .6 * key$width)
        heights.units <- c("grobheight", "lines", "lines")
        heights.data <- list(labelsGrob, NULL, NULL)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units, data = heights.data),
                        widths = unit(widths.x, widths.units),
                        respect = TRUE)
        
        key.gf <- frameGrob(layout = key.layout, vp = vp)

        key.gf <- placeGrob(key.gf,
                            rectGrob(y = rep(.5, length(reccentre)), 
                                     x = reccentre,
                                     default.units = "native",
                                     vp = viewport(xscale = atrange),
                                     width = recdim, 
                                     gp = gpar(fill = key$col,  col = NULL)),
                            row = 3, col = 2)
        
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd)),
                            row = 3, col = 2)


        key.gf <- placeGrob(frame = key.gf, 
                            segmentsGrob(y0 = rep(0, length(labscat)),
                                         x0 = labscat,
                                         y1 = rep(.4, length(labscat)),
                                         x1 = labscat,
                                         vp = viewport(xscale = atrange),
                                         default.units = "native",
                                         gp =
                                         gpar(col = axis.line$col,
                                              lty = axis.line$lty,
                                              lwd = axis.line$lwd)),
                            row = 2, col = 2)
        
        key.gf <- placeGrob(key.gf,
                            labelsGrob, 
                            row = 1, col = 2)

    }
    else if (key$space == "bottom") {


        labelsGrob <-
            textGrob(label = labels,
                     y = rep(1, length(labscat)),
                     x = labscat,
                     vp = viewport(xscale = atrange),
                     default.units = "native",
                     check.overlap = check.overlap,
                     just = c("center","top"),
                     gp =
                     gpar(col = col,
                          cex = cex,
                          fontfamily = fontfamily,
                          fontface = chooseFace(fontface, font)))

        widths.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        widths.units <- rep("null", 3)

        heights.x <- c(.6 * key$width, .6, 1)
        heights.units <- c("lines", "lines", "grobheight")
        heights.data <- list(NULL, NULL, labelsGrob)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units, data = heights.data),
                        widths = unit(widths.x, widths.units),
                        respect = TRUE)
        
        key.gf <- frameGrob(layout = key.layout, vp = vp)

        key.gf <- placeGrob(key.gf,
                            rectGrob(y = rep(.5, length(reccentre)), 
                                     x = reccentre,
                                     default.units = "native",
                                     vp = viewport(xscale = atrange),
                                     width = recdim, 
                                     gp = gpar(fill = key$col,  col = NULL)),
                            row = 1, col = 2)

        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd)),
                            row = 1, col = 2)

        key.gf <- placeGrob(frame = key.gf, 
                            segmentsGrob(y0 = rep(1, length(labscat)),
                                         x0 = labscat,
                                         y1 = rep(.6, length(labscat)),
                                         x1 = labscat,
                                         vp = viewport(xscale = atrange),
                                         default.units = "native",
                                         gp =
                                         gpar(col = axis.line$col,
                                              lty = axis.line$lty,
                                              lwd = axis.line$lwd)),
                            row = 2, col = 2)
        
        key.gf <- placeGrob(key.gf,
                            labelsGrob, 
                            row = 3, col = 2)

    }


    if (draw)
        grid.draw(key.gf)
    
    key.gf
}











calculateGridLayout <- function(x,
                                rows.per.page, cols.per.page,
                                number.of.cond,
                                panel.height, panel.width,

                                main, sub,
                                xlab, ylab,

                                x.alternating, y.alternating,
                                x.relation.same, y.relation.same,

                                xaxis.rot, yaxis.rot,
                                xaxis.cex, yaxis.cex,

                                par.strip.text,

                                legend)
    ## x: the trellis object
{


    ## The idea here is to create a layout with proper widths and
    ## heights (representing the requisite amounts of space required
    ## for different components of the plot -- see descriptions below)
    ## using the various units available in grid.

    ## Most of these components are fairly easy to define, with one
    ## exception -- namely those that involve axis labels. For
    ## instance, one (or more) _columns_ would usually contain the
    ## y-axis tick-labels. The width of this column is determined by
    ## ALL the y-labels; basically, the width of the column would be
    ## the MAXIMUM of the widths of the individual labels.

    ## This is in general not an easy problem, since relative width
    ## depends on the font used (also perhaps the device). Till
    ## lattice version 0.6, this was dealt with naively by treating
    ## the label with highest nchar() to be the widest. Unfortunately,
    ## this was no longer possible with labels that were
    ## expressions. So, after grid.text started supporting expression
    ## markups, the method of determining widths/heights for tick
    ## labels has changed. The new method essentially calculates the
    ## MAXIMUM of several grid UNIT objects (using calls like
    ## max(unit(...))) .

    ## The problem with this is that it is impossible to define the
    ## 'units' argument of those parts of the eventual layout when
    ## it's first being defined (it is not "null", "lines" or anything
    ## like that). So, those parts are calculated as separate units
    ## (via max.unit) and then inserted into the layout later.

    ## All this makes the code a bit difficult to follow. I just hope
    ## this gives some hints to whoever (probably me!) tries to
    ## decipher the following code on some later date.


    n.row <- rows.per.page * (number.of.cond + 3) + (rows.per.page-1) + 11
    ##       ^^^^^^^^^^^      ^^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^    ^^
    ##          panels         rows per panel           between     see below
    ##               (2 for axes/ticks when relation!=same)

    ## the 11 things are as follows (top to bottom)
    ## 1/2 line space at top
    ## main
    ## key
    ## tick labels
    ## ticks
    ##
    ##   actual panels
    ##
    ## ticks
    ## tick labels
    ## xlab
    ## key
    ## sub
    ## 1/2 line space at bottom

    n.col <- 3 * cols.per.page + (cols.per.page-1) + 9 # similar

    ## the 9 things are as follows (left to right)
    ## 1/2 line space at left
    ## key
    ## ylab
    ## tick labels
    ## ticks
    ##
    ##   actual panels
    ##
    ## ticks
    ## tick labels
    ## key
    ## 1/2 line space at right

    ## The next block applies when aspect is anything other than
    ## "fill", which means that the aspect ratio of panels are
    ## fixed. In grid terms, this means that the 'respect' argument
    ## has to be true for elements of the layout that correspond to
    ## panels.

    ## Earlier code used to set all respect entries to be TRUE in such
    ## cases (no need for matrices then), but that fails with the
    ## complicated layout necessitated by expressions (see above).


    layout.respect <- !x$aspect.fill

    if (layout.respect) {
        layout.respect <- matrix(0, n.row, n.col)

        layout.respect[number.of.cond + 6 + (1:rows.per.page - 1) *
                       (number.of.cond+4), (1:cols.per.page - 1)*4 +
                       8] <- 1

    }

    if(cols.per.page > 1)
        x.between <- rep(x$x.between, length = cols.per.page - 1)
    if(rows.per.page > 1) 
        y.between <- rep(x$y.between, length = rows.per.page - 1)
    





    ## see ?unit before trying to follow this. 

    heights.x <- rep(1, n.row)
    heights.units <- rep("lines", n.row)
    heights.data <- as.list(1:n.row)

    widths.x <- rep(1, n.col)
    widths.units <- rep("lines", n.col)
    widths.data <- as.list(1:n.col) 

    ## fine tuning heights:


    heights.x[number.of.cond + 6 + (1:rows.per.page - 1) * (number.of.cond+4)] <-
        panel.height[[1]] # for the panels
    heights.units[number.of.cond + 6 + (1:rows.per.page - 1) * (number.of.cond+4)] <-
        panel.height[[2]]
    ## was "null" # for the panels

    heights.x[number.of.cond + 7 + (1:rows.per.page - 1) * (number.of.cond+4)] <- 0
    ## This is for the x-axis ticks just below each panel if relation!="same"
    heights.x[number.of.cond + 8 + (1:rows.per.page - 1) * (number.of.cond+4)] <- 0
    ## This is for the x-axis labels just below each panel if relation!="same"

    heights.x[4] <- 0
    heights.x[5] <- 0 # tick axes/labels
    heights.x[n.row-4] <- 0
    heights.x[n.row-5] <- 0



    if (rows.per.page > 1)
        heights.x[number.of.cond + 9 +
                  ((if (x$as.table) 1:(rows.per.page-1)
                  else (rows.per.page-1):1)
                   - 1)*(number.of.cond+4)] <-
                       y.between
    ## y-between


    

    heights.x[1] <- 0.5

    if (is.null(main))
        heights.x[2] <- 0
    else
    {
        heights.x[2] <- 2 * main$cex
        heights.units[2] <-  "strheight"
        heights.data[[2]] <- main$lab
    }



    heights.x[n.row] <- 0.5

    if (is.null(sub))
        heights.x[n.row-1] <- 0
    else
    {
        heights.x[n.row-1] <- 2 * sub$cex
        heights.units[n.row-1] <-  "strheight"
        heights.data[[n.row-1]] <- sub$lab
    }

    heights.x[3] <- 0 # for the key
    heights.x[n.row-2] <- 0 # key

    ## next part of the code decides how much space to leave for
    ## x tick-labels. This wasn't that bad earlier, but has become
    ## complicated to support expression-style labels. Not sure if
    ## there's a better way (would definitely need a lot of
    ## redesigning), something to look at later.

    heights.insertlist.position <- 0
    heights.insertlist.unit <- unit(1, "null")

    ## both these dummies, since there is no unit(numeric(0)). These
    ## are necessary for calculating space for axis
    ## labels. Unfortunately this makes the code complicated



    ## All the upcoming horrendous code about labels is only to
    ## determine how much space to leave for them. Much of these
    ## calcualtions will be repeated later before actually drawing
    ## them. Maybe the two can be combined ?

    if (x$x.scales$draw) {

        if (x.relation.same) {

            lab <- 
                calculateAxisComponents(x = x$x.limits,
                                        at = x$x.scales$at,
                                        labels = x$x.scales$lab,
                                        logsc = x$x.scales$log,
                                        #have.log = have.xlog,
                                        #logbase = xlogbase,
                                        #logpaste = xlogpaste,
                                        abbreviate = x$x.scales$abbr,
                                        minlength = x$x.scales$minl,
                                        format.posixt = x$x.scales$format,
                                        n = x$x.scales$tick.number)$lab


            
            if (is.character(lab)) 
                strbar <- as.list(lab)
            else if (is.expression(lab)) {
                strbar <- list() ## will contain list for max.unit data
                for (ss in seq(along = lab))
                    strbar <- c(strbar, list(lab[ss]))
            }
            else stop("Invalid value for labels")

            heights.x[5] <- 0.5 + max(0.001, x$x.scales$tck[2]) * 0.3
            ## tck = 2 is .5 lines + .6 lines
            heights.x[n.row-5] <- 0.5 + max(0.001, x$x.scales$tck[1]) * 0.3

            if (any(x.alternating==2 | x.alternating==3)) {

                if (xaxis.rot[2]  %in% c(0, 180)) {

                    heights.insertlist.position <- c(heights.insertlist.position, 4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[2],
                                            length(strbar)), "strheight", strbar)))
                }
                else {
                    heights.insertlist.position <- c(heights.insertlist.position, 4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[2] * abs(sin(xaxis.rot[2] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                }
            }

            if (any(x.alternating==1 | x.alternating==3)) {

                if (xaxis.rot[1]  %in% c(0, 180)) {
                    
                    heights.insertlist.position <- c(heights.insertlist.position, n.row-4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[1],
                                            length(strbar)), "strheight", strbar)))
                }

                else {

                    heights.insertlist.position <- c(heights.insertlist.position, n.row-4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[1] * abs(sin(xaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                }
            }
        }
        else { # relation != same


            ## Basically need to allocate space for the tick labels.
            ## Technically, could have different heights for different
            ## rows, but don't want to go there (probably won't look
            ## good anyway). So, boils down to finding all the
            ## labels. If at is a list, have to go through all (for
            ## each panel). If not, still have to go through
            ## all. Could save some work if at is explicitly
            ## specified, but ignoring that for now.


            labelChars <- character(0)
            labelExprs <- expression(0)
            for (i in seq(along = x$x.limits)) {
                lab <-
                    calculateAxisComponents(x = x$x.limits[[i]],
                                            at = if (is.list(x$x.scales$at)) x$x.scales$at[[i]] else x$x.scales$at,
                                            labels = if (is.list(x$x.scales$lab)) x$x.scales$lab[[i]] else x$x.scales$lab,
                                            logsc = x$x.scales$log,
                                            #have.log = have.xlog,
                                            #logbase = xlogbase,
                                            #logpaste = xlogpaste,
                                            abbreviate = x$x.scales$abbr,
                                            minlength = x$x.scales$minl,
                                            n = x$x.scales$tick.number,
                                            format.posixt = x$x.scales$format)$lab
                if (is.character(lab)) 
                    labelChars <- c(labelChars, lab)
                else if (is.expression(lab))
                    labelExprs <- c(labelExprs, lab)
            }
            labelChars <- unique(labelChars)

            strbar <- list() ## will contain list for max.unit data
            for (ss in labelChars)
                strbar <- c(strbar, list(ss))
            for (ss in seq(along = labelExprs))
                strbar <- c(strbar, list(labelExprs[ss]))

            if (xaxis.rot[1] %in% c(0, 180)) {

                heights.x[number.of.cond + 7 + (1:rows.per.page - 1)*(number.of.cond+4)] <-
                    max(0.001, x$x.scales$tck[1]) * 0.3  ## tck = 1 -- 0.3 lines

                heights.insertlist.position <-
                    c(heights.insertlist.position,
                      number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4))
                for (i in 1:rows.per.page)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.5 * xaxis.cex[1],
                                            length(strbar)), "strheight", strbar)))

            }
            else {

                heights.x[number.of.cond + 7 + (1:rows.per.page - 1)*(number.of.cond+4)] <-
                    max(0.001, x$x.scales$tck[1]) * 0.3

                ##if (is.logical(x$x.scales$at)) {
                ##    heights.x[number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4)] <-
                ##        1.1 * xaxis.cex * abs(sin(xaxis.rot * pi /180))
                ##    heights.units[number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4)] <- "strwidth"
                ##    heights.data[number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4)] <- which.name
                ##}
                ##else {
                heights.insertlist.position <-
                    c(heights.insertlist.position,
                      number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4))
                for (i in 1:rows.per.page)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.5 * xaxis.cex[1] * abs(sin(xaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                ##}
            }
        }
    }



    ## xlab
    if (is.null(xlab))
        heights.x[n.row-3] <- 0.2 
    else
    {
        heights.x[n.row-3] <- 2 * xlab$cex
        heights.units[n.row-3] <-  "strheight"
        heights.data[[n.row-3]] <- xlab$lab
    }

    ## space for strips
    for(crr in 1:number.of.cond)
        heights.x[number.of.cond + 6 + (1:rows.per.page - 1)*(number.of.cond+4) - crr] <-
            if (is.logical(x$strip)) 0  # which means strip = F, strips not to be drawn
            else 1.1 * par.strip.text$cex * par.strip.text$lines





    ## fine tuning widths:
    ##----------------------------------------------------------------------------------
    
    ## ylab
    if (is.null(ylab))
        widths.x[3] <- 0.2
    else
    {
        widths.x[3] <- 2 * ylab$cex
        widths.units[3] <-  "strheight"
        widths.data[[3]] <- ylab$lab
    }


    widths.x[(1:cols.per.page - 1)*4 + 8] <-
        panel.width[[1]] # for the panels
    widths.units[(1:cols.per.page - 1)*4 + 8] <-
        panel.width[[2]] # for the panels
    ## was "null"


    widths.x[(1:cols.per.page - 1)*4 + 7] <- 0
    widths.x[(1:cols.per.page - 1)*4 + 6] <- 0
    ## For y-axes labels and ticks to the left of each panel when relation != "same"
    ## (might change later)

    widths.x[4] <- 0
    widths.x[5] <- 0 #ticks/labels
    widths.x[n.col-2] <- 0
    widths.x[n.col-3] <- 0

    if (cols.per.page > 1)
        widths.x[(1:(cols.per.page-1) - 1)*4 + 9] <- x.between
    ## x-between

    widths.x[1] <- 0.5
    widths.x[n.col] <- 0.5
    widths.x[2] <- 0 # key - left
    widths.x[n.col-1] <- 0 # key - right

    ## next part of the code decides how much space to leave for y
    ## tick-labels. This wasn't that bad earlier, but has become
    ## complicated to support expression-style labels. Not sure if
    ## there's a better way (would definitely need a lot of
    ## redesigning), something to look at later.

    widths.insertlist.position <- 0
    widths.insertlist.unit <- unit(1, "null")
    ## both these dummies, since there is no unit(numeric(0)). These
    ## are necessary for calculating space for axis
    ## labels. Unfortunately this makes the code complicated

    if (x$y.scales$draw) {
        
        if (y.relation.same) {

            lab <- 
                calculateAxisComponents(x = x$y.limits,
                                        at = x$y.scales$at,
                                        labels = x$y.scales$lab,
                                        logsc = x$y.scales$log,
                                        #have.log = have.ylog,
                                        #logbase = ylogbase,
                                        #logpaste = ylogpaste,
                                        abbreviate = x$y.scales$abbr,
                                        minlength = x$y.scales$minl,
                                        n = x$y.scales$tick.number,
                                        format.posixt = x$y.scales$format)$lab


            if (is.character(lab)) 
                strbar <- as.list(lab)
            else if (is.expression(lab)) {
                strbar <- list() ## will contain list for max.unit data
                for (ss in seq(along = lab))
                    strbar <- c(strbar, list(lab[ss]))
            }
            else {
                stop("Invalid value for labels")
            }

            widths.x[5] <- 0.5 + max(0.001, x$y.scales$tck[1]) * 0.3
            ## tck = 2 is .5 lines + .6 lines


##FIXME
            
            ## WAS : widths.x[n.col-3] <- max(1, x$y.scales$tck[2]) * 0.5
            ## not sure why
            ## changed to:

            widths.x[n.col-3] <- 0.5 + max(0.001, x$y.scales$tck[2]) * 0.3



            if (any(y.alternating==1 | y.alternating==3)) {

                if (abs(yaxis.rot[1]) == 90) {

                    widths.insertlist.position <- c(widths.insertlist.position, 4)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(1.0 * rep(yaxis.cex[1],
                                                  length(strbar)), "strheight", data = strbar)))
                }
                
                else {

                    widths.insertlist.position <- c(widths.insertlist.position, 4)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.0 * yaxis.cex[1] * abs(cos(yaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                }
            }

            if (any(y.alternating==2 | y.alternating==3)) {

                if (abs(yaxis.rot[2]) == 90) {
                    widths.insertlist.position <- c(widths.insertlist.position, n.col-2)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.0 * yaxis.cex[2],
                                            length(strbar)), "strheight", strbar)))
                }

                else {
                    widths.insertlist.position <- c(widths.insertlist.position, n.col-2)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.0 * yaxis.cex[2] * abs(cos(yaxis.rot[2] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                    
                }
            }
        }
        else { # relation != same

            ## See comments for x-scales above
            
            labelChars <- character(0)
            labelExprs <- expression(0)
            for (i in seq(along = x$y.limits)) {
                lab <-
                    calculateAxisComponents(x = x$y.limits[[i]],
                                            at = if (is.list(x$y.scales$at)) x$y.scales$at[[i]] else x$y.scales$at,
                                            labels = if (is.list(x$y.scales$lab)) x$y.scales$lab[[i]] else x$y.scales$lab,
                                            logsc = x$y.scales$log,
                                            #have.log = have.ylog,
                                            #logbase = ylogbase,
                                            #logpaste = ylogpaste,
                                            abbreviate = x$y.scales$abbr,
                                            minlength = x$y.scales$minl,
                                            n = x$y.scales$tick.number,
                                            format.posixt = x$y.scales$format)$lab
                if (is.character(lab)) 
                    labelChars <- c(labelChars, lab)
                else if (is.expression(lab))
                    labelExprs <- c(labelExprs, lab)
            }
            labelChars <- unique(labelChars)

            strbar <- list() ## will contain list for max.unit data
            for (ss in labelChars)
                strbar <- c(strbar, list(ss))
            for (ss in seq(along = labelExprs))
                strbar <- c(strbar, list(labelExprs[ss]))


            if (abs(yaxis.rot[1]) == 90) {

                widths.x[(1:cols.per.page - 1)*4 + 7] <- 
                    max(0.001, x$y.scales$tck[1]) * 0.3  ## tck = 1 -- 0.3 lines

                widths.insertlist.position <-
                    c(widths.insertlist.position,
                      (1:cols.per.page - 1) * 4 + 6)
                for (i in 1:cols.per.page)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.5 * yaxis.cex[1],
                                            length(strbar)), "strheight", strbar)))

            }
            else {

                widths.x[(1:cols.per.page - 1)*4 + 7] <- 
                    max(0.001, x$y.scales$tck[1]) * 0.3

                widths.insertlist.position <-
                    c(widths.insertlist.position, (1:cols.per.page - 1)*4 + 6)
                for (i in 1:cols.per.page)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.2 * yaxis.cex[1] * abs(cos(yaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
            }
        }
    }



    if (!is.null(legend))
    {
        ## allocate space as necessary

        if ("left" %in% names(legend))
        {
            widths.x[2] <- 1.2
            widths.units[2] <- "grobwidth"
            widths.data[[2]] <- legend$left$obj
        }
        if ("right" %in% names(legend))
        {
            widths.x[n.col-1] <- 1.2
            widths.units[n.col-1] <- "grobwidth"
            widths.data[[n.col-1]] <- legend$right$obj
        }
        if ("top" %in% names(legend))
        {
            heights.x[3] <- 1.2
            heights.units[3] <- "grobheight"
            heights.data[[3]] <- legend$top$obj
        }
        if ("bottom" %in% names(legend))
        {
            heights.x[n.row-2] <- 1.2
            heights.units[n.row-2] <- "grobheight"
            heights.data[[n.row-2]] <- legend$bottom$obj
        }
    }







#     if (!is.null(x$key) || !is.null(x$colorkey)) {
            
#         if (key.space == "left") {
#             widths.x[2] <- 1.2
#             widths.units[2] <- "grobwidth"
#             widths.data[[2]] <- key.gf
#         }
#         else if (key.space == "right") {
#             widths.x[n.col-1] <- 1.2
#             widths.units[n.col-1] <- "grobwidth"
#             widths.data[[n.col-1]] <- key.gf
#         }
#         else if (key.space == "top") {
#             heights.x[3] <- 1.2
#             heights.units[3] <- "grobheight"
#             heights.data[[3]] <- key.gf
#         }
#         else if (key.space == "bottom") {
#             heights.x[n.row-2] <- 1.2
#             heights.units[n.row-2] <- "grobheight"
#             heights.data[[n.row-2]] <- key.gf
#         }
        
#     }
    

    ## Having determined heights and widths, now construct the layout:

    layout.heights <- unit(heights.x, heights.units, data=heights.data)
    if (length(heights.insertlist.position)>1)
        for (indx in 2:length(heights.insertlist.position))
            layout.heights <-
                rearrangeUnit(layout.heights, heights.insertlist.position[indx],
                              heights.insertlist.unit[indx])
    

    layout.widths <- unit(widths.x, widths.units, data=widths.data)
    if (length(widths.insertlist.position)>1)
        for (indx in 2:length(widths.insertlist.position))
            layout.widths <-
                rearrangeUnit(layout.widths, widths.insertlist.position[indx],
                              widths.insertlist.unit[indx])
    
    page.layout <- grid.layout(nrow = n.row, ncol = n.col,
                               widths = layout.widths,
                               heights = layout.heights,
                               respect = layout.respect)


    page.layout
}
















evaluate.legend <- function(legend)
{
    if (is.null(legend)) return(NULL)
    for (i in seq(along = legend))
    {
        fun <- legend[[i]]$fun
        fun <- 
            if (is.function(fun)) fun 
            else if (is.character(fun)) get(fun)
            else eval(fun)  ## in case fun is a grob (is this OK?)
        if (is.function(fun)) fun <- do.call("fun", legend[[i]]$args)
        legend[[i]]$obj <- fun
        legend[[i]]$args <- NULL
        legend[[i]]$fun <- NULL
    }
    legend
}





























print.trellis <-
    function(x, position, split, more = FALSE,
             newpage = TRUE,
             panel.height = list(1, "null"),
             panel.width = list(1, "null"),
             ...)
{
    if (is.null(dev.list())) trellis.device()
    else if (is.null(trellis.par.get()))
        trellis.device(device = .Device, new = FALSE)

    ## if necessary, save current settings and apply temporary
    ## settings in x$par.settings

    if (!is.null(x$par.settings))
    {
        opars <- trellis.par.get()
        lset(x$par.settings)
    }

    bg = trellis.par.get("background")$col
    new <- TRUE
    if (get(".lattice.print.more", envir=.LatticeEnv) || !newpage) new <- FALSE
    assign(".lattice.print.more", more, envir=.LatticeEnv)
    usual  <- (missing(position) & missing(split))
    ##if (!new && usual)
    ##    warning("more is relevant only when split/position is specified")

    fontsize.text <- trellis.par.get("fontsize")$text
    
    if (!missing(position)) {
        if (length(position)!=4) stop("Incorrect value of position")
        if (new)
        {
            grid.newpage()
            grid.rect(gp = gpar(fill = bg, col = "transparent"))
        }
        pushViewport(viewport(x = position[1], y = position[2],
                              width = position[3] - position[1],
                              height = position[4] - position[2],
                              just = c("left","bottom")))
        
        if (!missing(split))
        {
            if (length(split)!=4) stop("Incorrect value of split")
            pushViewport(viewport(layout = grid.layout(nrow=split[4], ncol = split[3])))
            pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1]))
        }
    }
    
    
    else if (!missing(split)) {
        
        if (length(split)!=4) stop("Incorrect value of split")
        if (new)
        {
            grid.newpage()
            grid.rect(gp = gpar(fill = bg, col = "transparent"))
        }
        pushViewport(viewport(layout = grid.layout(nrow=split[4], ncol = split[3])))
        pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1]))
    }









    ## reordering stuff


    ## FLAG: potentially changes x

    ## the following definitions are already made in high level functions
#     if (is.null(x$index.cond)) {
#         x$index.cond <-
#             vector(mode = "list",
#                    length = length(x$condlevels))
#         for (i in seq(along = x$condlevels))
#             x$index.cond[[i]] <- seq(along = x$condlevels[[i]])
#     }
#     if (is.null(x$perm.cond))
#         x$perm.cond <- seq(length = length(x$condlevels))

    ## but maybe a validity check (to ensure current values make
    ## sense) would be useful here (or, wherever it can be changed)








    ## order.cond will be a multidimensional array, with
    ## length(dim(order.cond)) = number of conditioning
    ## variables. It's a numeric vector 1:(number.of.panels), with
    ## dim() = c(nlevels(g1), ..., nlevels(gn)), where g1, ..., gn are
    ## the conditioning variables.

    ## manipulating order.cond has 2 uses. Using normal indexing, the
    ## order of plots within a conditioning variable can be altered,
    ## or only a subset of the levels used. Also, using aperm, the
    ## order of conditioning can be altered.

    ## the information required to make the appropriate permutation
    ## and indexing is in the components index.cond and perm.cond of
    ## the trellis object

    order.cond <- seq(length = prod(sapply(x$condlevels, length)))
    dim(order.cond) <- sapply(x$condlevels, length)

    ## first subset, then permute
    order.cond <- do.call("[", c(list(order.cond), x$index.cond, list(drop = FALSE)))
    order.cond <- aperm(order.cond, perm = x$perm.cond)

    ## order.cond will be used as indices for (exactly) the following

    ## 1. panel.args
    ## 2. x.limits
    ## 3. y.limits

    ## may need x$(subset|perm).cond later for strip drawing

    cond.max.level <- dim(order.cond)
    number.of.cond <- length(cond.max.level)

    panel.layout <-
        compute.layout(x$layout, cond.max.level, skip = x$skip)

    panel <- # shall use "panel" in do.call
        if (is.function(x$panel)) x$panel 
        else if (is.character(x$panel)) get(x$panel)
        else eval(x$panel)

    strip <- 
        if (is.function(x$strip)) x$strip 
        else if (is.character(x$strip)) get(x$strip)
        else eval(x$strip)

    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")


    ## make sure aspect ratio is preserved for aspect != "fill" but
    ## this may not always be what's expected. In fact, aspect should
    ## be "fill" whenever panel.width or panel.height are non-default.

    ## panel.width <- 1
    if (!x$aspect.fill)
        panel.height[[1]] <- x$aspect.ratio * panel.width[[1]]





    ## Evaluate the legend / key grob(s): 

    legend <- evaluate.legend(x$legend)

    ## legend is now a list of `grob's along with placement info



    xaxis.lty <-
        if (is.logical(x$x.scales$lty)) axis.line$lty
        else x$x.scales$lty
    xaxis.lwd <-
        if (is.logical(x$x.scales$lwd)) axis.line$lwd
        else x$x.scales$lwd
    xaxis.col.line <-
        if (is.logical(x$x.scales$col.line)) axis.line$col
        else x$x.scales$col.line
    xaxis.col.text <-
        if (is.logical(x$x.scales$col)) axis.text$col
        else x$x.scales$col
    xaxis.font <-
        if (is.logical(x$x.scales$font)) axis.text$font
        else x$x.scales$font
    xaxis.fontface <-
        if (is.logical(x$x.scales$fontface)) axis.text$fontface
        else x$x.scales$fontface
    xaxis.fontfamily <-
        if (is.logical(x$x.scales$fontfamily)) axis.text$fontfamily
        else x$x.scales$fontfamily
    xaxis.cex <-
        if (is.logical(x$x.scales$cex)) rep(axis.text$cex, length = 2)
        else x$x.scales$cex
    xaxis.rot <-
        if (is.logical(x$x.scales$rot)) c(0, 0)
        else x$x.scales$rot



    yaxis.lty <-
        if (is.logical(x$y.scales$lty)) axis.line$lty
        else x$y.scales$lty
    yaxis.lwd <-
        if (is.logical(x$y.scales$lwd)) axis.line$lwd
        else x$y.scales$lwd
    yaxis.col.line <-
        if (is.logical(x$y.scales$col.line)) axis.line$col
        else x$y.scales$col.line
    yaxis.col.text <-
        if (is.logical(x$y.scales$col)) axis.text$col
        else x$y.scales$col
    yaxis.font <-
        if (is.logical(x$y.scales$font)) axis.text$font
        else x$y.scales$font
    yaxis.fontface <-
        if (is.logical(x$y.scales$fontface)) axis.text$fontface
        else x$y.scales$fontface
    yaxis.fontfamily <-
        if (is.logical(x$y.scales$fontfamily)) axis.text$fontfamily
        else x$y.scales$fontfamily
    yaxis.cex <-
        if (is.logical(x$y.scales$cex)) rep(axis.text$cex, length = 2)
        else x$y.scales$cex
    yaxis.rot <-
        if (!is.logical(x$y.scales$rot)) x$y.scales$rot
        else if (x$y.scales$relation != "same" && is.logical(x$y.scales$labels)) c(90, 90)
        else c(0, 0)




    strip.col.default.bg <-
        rep(trellis.par.get("strip.background")$col,length=number.of.cond)
    strip.col.default.fg <-
        rep(trellis.par.get("strip.shingle")$col,length=number.of.cond)



    ## Start layout calculations when only number of panels per page
    ## is pecified (this refers to the layout argument, not grid
    ## layouts)

    if (panel.layout[1] == 0) { # using device dimensions to

        ddim <- par("din") # calculate default layout
        device.aspect <- ddim[2] / ddim[1]
        panel.aspect <- panel.height[[1]] / panel.width[[1]]

        plots.per.page <- panel.layout[2]
        m <- max (1, round(sqrt(panel.layout[2] * device.aspect / panel.aspect)))
        ## changes made to fix bug (PR#1744)
        n <- ceiling(plots.per.page/m)
        m <- ceiling(plots.per.page/n)
        panel.layout[1] <- n
        panel.layout[2] <- m

    }
    else plots.per.page <- panel.layout[1] * panel.layout[2] 

    ## End layout calculations


    cols.per.page <- panel.layout[1]
    rows.per.page <- panel.layout[2]
    number.of.pages <- panel.layout[3]

    skip <- rep(x$skip, length = number.of.pages * rows.per.page * cols.per.page)

    x.alternating <- rep(x$x.scales$alternating, length = cols.per.page)
    y.alternating <- rep(x$y.scales$alternating, length = rows.per.page)
    x.relation.same <- x$x.scales$relation == "same"
    y.relation.same <- x$y.scales$relation == "same"

    ## get lists for main, sub, xlab, ylab

    main <- getLabelList(x$main, trellis.par.get("par.main.text"))
    sub <- getLabelList(x$sub, trellis.par.get("par.sub.text"))
    xlab <- getLabelList(x$xlab, trellis.par.get("par.xlab.text"), x$xlab.default)
    ylab <- getLabelList(x$ylab, trellis.par.get("par.ylab.text"), x$ylab.default)


    ## get par.strip.text

    par.strip.text <- trellis.par.get("add.text")
    par.strip.text$lines <- 1
    if (!is.null(x$par.strip.text)) 
        par.strip.text[names(x$par.strip.text)] <- x$par.strip.text



    ## Shall calculate the per page Grid layout now:

    ## this layout will now be used for each page (this is quite
    ## complicated and unfortunately very convoluted)


    page.layout <- calculateGridLayout(x,
                                       rows.per.page, cols.per.page,
                                       number.of.cond,
                                       panel.height, panel.width,
                                       main, sub,
                                       xlab, ylab,
                                       x.alternating, y.alternating,
                                       x.relation.same, y.relation.same,
                                       xaxis.rot, yaxis.rot,
                                       xaxis.cex, yaxis.cex,
                                       par.strip.text,
                                       legend)


    n.row <- layoutNRow(page.layout)
    n.col <- layoutNCol(page.layout)


    ## commence actual plotting

    
    cond.current.level <- rep(1, number.of.cond)

    ##   this vector represents the combination of levels of the
    ##   conditioning variables for the current panel.

    
    panel.counter <- 0

    ## panel.counter used as an optional argument to the panel
    ## function. Sequential counter keeping track of which panel is
    ## being drawn

    for(page.number in 1:number.of.pages)
    {
        if (!any(cond.max.level - cond.current.level < 0)) {
            
            if (usual) {
                if (new) grid.newpage()
                grid.rect(gp = gpar(fill = bg, col = "transparent"))
                new <- TRUE
            }

            pushViewport(viewport(layout = page.layout,
                                  gp =
                                  gpar(fontsize = fontsize.text)))

            if (!is.null(main))
                grid.text(label = main$label,
                          gp =
                          gpar(col = main$col,
                               fontfamily = main$fontfamily,
                               fontface = chooseFace(main$fontface, main$font),
                               cex = main$cex),
                          vp = viewport(layout.pos.row = 2))

            if (!is.null(sub))
                grid.text(label = sub$label,
                          gp =
                          gpar(col = sub$col,
                               fontfamily = sub$fontfamily,
                               fontface = chooseFace(sub$fontface, sub$font),
                               cex = sub$cex),
                          vp = viewport(layout.pos.row = n.row-1))

            if (!is.null(xlab))
                grid.text(label = xlab$label,
                          gp =
                          gpar(col = xlab$col,
                               fontfamily = xlab$fontfamily,
                               fontface = chooseFace(xlab$fontface, xlab$font),
                               cex = xlab$cex), 
                          vp = viewport(layout.pos.row = n.row - 3, layout.pos.col = c(6, n.col - 4)))

            if (!is.null(ylab))
                grid.text(label = ylab$label, rot = 90,
                          gp =
                          gpar(col = ylab$col,
                               fontfamily = ylab$fontfamily,
                               fontface = chooseFace(ylab$fontface, ylab$font),
                               cex = ylab$cex),
                          vp = viewport(layout.pos.col = 3, layout.pos.row = c(6, n.row - 6)))


            for (row in 1:rows.per.page)
                for (column in 1:cols.per.page)
                {
                    if (!any(cond.max.level-cond.current.level<0) &&
                        (row-1) * cols.per.page + column <= plots.per.page &&
                        !skip[(page.number-1) * rows.per.page * cols.per.page +
                              (row-1) * cols.per.page + column] )
                    {


                        ##panel.number should be same as order.cond[cond.current.level]
                        ##                                          ^^^^^^^^^^^^^^^^^^
                        ##                                          (length not fixed)

                        panel.number <- 
                            do.call("[", c(list(x = order.cond), as.list(cond.current.level)))

                        ## this index retrieves the appropriate entry
                        ## of panel.args and [xy].limits. It has to be
                        ## this way because otherwise non-trivial
                        ## orderings will not work.

                        ## But we should also have a simple
                        ## incremental counter that may be used as a
                        ## panel function argument

                        panel.counter <- panel.counter + 1

                        ## this will also be used to name the panel
                        ## viewport for later accessing

                        actual.row <- if (x$as.table)
                            (rows.per.page-row+1) else row

                        ## this gives the row position from the bottom


                        pos.row <- 6 + number.of.cond + 
                            (rows.per.page - actual.row) *
                                (number.of.cond + 4)
                        pos.col <- (column-1) * 4 + 8


                        xlabelinfo <-
                            calculateAxisComponents(x =
                                                    if (x.relation.same) x$x.limits
                                                    else x$x.limits[[panel.number]],
                                                    at =
                                                    if (is.list(x$x.scales$at)) x$x.scales$at[[panel.number]]
                                                    else x$x.scales$at,
                                                    labels =
                                                    if (is.list(x$x.scales$lab)) x$x.scales$lab[[panel.number]]
                                                    else x$x.scales$lab,
                                                    logsc = x$x.scales$log,
                                                    abbreviate = x$x.scales$abbr,
                                                    minlength = x$x.scales$minl,
                                                    n = x$x.scales$tick.number,
                                                    format.posixt = x$x.scales$format)

                        ylabelinfo <-
                            calculateAxisComponents(x =
                                                    if (y.relation.same) x$y.limits
                                                    else x$y.limits[[panel.number]],
                                                    at =
                                                    if (is.list(x$y.scales$at)) x$y.scales$at[[panel.number]]
                                                    else x$y.scales$at,
                                                    labels =
                                                    if (is.list(x$y.scales$lab)) x$y.scales$lab[[panel.number]]
                                                    else x$y.scales$lab,
                                                    logsc = x$y.scales$log,
                                                    abbreviate = x$y.scales$abbr,
                                                    minlength = x$y.scales$minl,
                                                    n = x$y.scales$tick.number,
                                                    format.posixt = x$y.scales$format)



                        xscale <- xlabelinfo$num.limit
                        yscale <- ylabelinfo$num.limit

                        pushViewport(viewport(layout.pos.row = pos.row,
                                              layout.pos.col = pos.col,
                                              xscale = xscale,
                                              yscale = yscale,
                                              clip = trellis.par.get("clip")$panel,
                                              name = paste("panel", panel.counter, sep = ".")))


                        pargs <- c(x$panel.args[[panel.number]],
                                   x$panel.args.common,
                                   list(panel.number = panel.number,
                                        panel.counter = panel.counter))

                        if (!("..." %in% names(formals(panel))))
                            pargs <- pargs[names(formals(panel))]
                        do.call("panel", pargs)


                                
                        grid.rect(gp =
                                  gpar(col = axis.line$col,
                                       lty = axis.line$lty,
                                       lwd = axis.line$lwd))

                        upViewport()

                        ## next few lines deal with drawing axes
                        ## as appropriate

                        ## when relation != same, axes drawn for
                        ## each panel:
                        
                        ## X-axis
                        if (!x.relation.same && x$x.scales$draw) {

                            axstck <- x$x.scales$tck

                            ok <- seq(along = xlabelinfo$at)

                            pushViewport(viewport(layout.pos.row = pos.row+1,
                                                  layout.pos.col = pos.col,
                                                  xscale = xscale))

                            
                            if (axstck[1] !=0 && any(ok))
                                grid.segments(y0 = unit(rep(1, sum(ok)), "npc"),
                                              y1 = unit(rep(1, sum(ok)), "npc") -
                                              unit(rep(0.3 * axstck[1], sum(ok)), "lines"),
                                              x0 = unit(xlabelinfo$at[ok], "native"),
                                              x1 = unit(xlabelinfo$at[ok], "native"),
                                              gp =
                                              gpar(col = xaxis.col.line,
                                                   lty = xaxis.lty,
                                                   lwd = xaxis.lwd))



                            upViewport()

                            if (any(ok))
                                grid.text(label = xlabelinfo$label[ok],
                                          x = unit(xlabelinfo$at[ok], "native"),
                                          y = unit(if (xaxis.rot[1] %in% c(0, 180)) .5 else .95, "npc"),
                                          ##y = unit(.95, "npc"),
                                          just = if (xaxis.rot[1] == 0) c("centre", "centre")
                                          else if (xaxis.rot[1] == 180) c("centre", "centre")
                                          else if (xaxis.rot[1] > 0)  c("right", "centre")
                                          else c("left", "centre"),
                                          rot = xaxis.rot[1],
                                          check.overlap = xlabelinfo$check.overlap,
                                          gp =
                                          gpar(col = xaxis.col.text,
                                               fontfamily = xaxis.fontfamily,
                                               fontface = chooseFace(xaxis.fontface, xaxis.font),
                                               cex = xaxis.cex[1]),
                                          vp = viewport(layout.pos.row = pos.row + 2,
                                          layout.pos.col = pos.col, xscale = xscale))

                        }
                        ## Y-axis
                        if (!y.relation.same && x$y.scales$draw) {

                            axstck <- x$y.scales$tck

                            ok <- seq(along = ylabelinfo$at)

                            pushViewport(viewport(layout.pos.row = pos.row,
                                                  layout.pos.col = pos.col-1,
                                                  yscale = yscale))


                            if (axstck[1] !=0 && any(ok))
                                grid.segments(x0 = unit(rep(1, sum(ok)), "npc"),
                                              x1 = unit(rep(1, sum(ok)), "npc") -
                                              unit(rep(0.3 * axstck[1], sum(ok)), "lines"),
                                              y0 = unit(ylabelinfo$at[ok], "native"),
                                              y1 = unit(ylabelinfo$at[ok], "native"),
                                              gp =
                                              gpar(col = yaxis.col.line,
                                                   lty = yaxis.lty,
                                                   lwd = yaxis.lwd))

                            upViewport()


                            if (any(ok))
                                grid.text(label = ylabelinfo$label[ok],
                                          y = unit(ylabelinfo$at[ok], "native"),
                                          x = unit(if ( abs(yaxis.rot[1]) == 90) .5 else .95, "npc"),
                                          ##y = unit(.95, "npc"),
                                          just = if (yaxis.rot[1] == 90) c("centre", "centre")
                                          else if (yaxis.rot[1] == -90) c("centre", "centre")
                                          else if (yaxis.rot[1] > -90 && yaxis.rot[1] < 90) c("right", "centre")
                                          else c("left", "centre"),
                                          rot = yaxis.rot[1],
                                          check.overlap = ylabelinfo$check.overlap,
                                          gp =
                                          gpar(col = yaxis.col.text,
                                               fontfamily = yaxis.fontfamily,
                                               fontface = chooseFace(yaxis.fontface, yaxis.font),
                                               cex = yaxis.cex[1]),
                                          vp = viewport(layout.pos.row = pos.row,
                                          layout.pos.col = pos.col-2, yscale = yscale))

                        }

                        ## When relation = same, axes drawn based on value of alternating
                        if (y.relation.same && x$y.scales$draw) {
                            
                            ## Y-axis to the left
                            if (column == 1) {

                                axstck <- x$y.scales$tck

                                ok <- seq(along = ylabelinfo$at)

                                pushViewport(viewport(layout.pos.row = pos.row,
                                                      layout.pos.col = pos.col-3,
                                                      yscale = yscale))

                                if (axstck[1] !=0 && any(ok))
                                    grid.segments(x0 = unit(rep(1, sum(ok)), "npc"),
                                                  x1 = unit(rep(1, sum(ok)), "npc") -
                                                  unit(rep(0.3 * axstck[1], sum(ok)), "lines"),
                                                  y0 = unit(ylabelinfo$at[ok], "native"),
                                                  y1 = unit(ylabelinfo$at[ok], "native"),
                                                  gp =
                                                  gpar(col = yaxis.col.line,
                                                       lty = yaxis.lty,
                                                       lwd = yaxis.lwd))

                                upViewport()

                                if (y.alternating[actual.row]==1 || y.alternating[actual.row]==3) 

                                    if (any(ok)) 

                                        grid.text(label = ylabelinfo$lab[ok],
                                                  y = unit(ylabelinfo$at[ok], "native"),
                                                  x = unit(if (abs(yaxis.rot[1]) == 90) .5 else 1, "npc"),
                                                  ##y = unit(rep(.95, sum(ok)), "npc"),
                                                  just = if (yaxis.rot[1] == -90) c("centre", "centre")
                                                  else if (yaxis.rot[1] == 90) c("centre", "centre")
                                                  else if (yaxis.rot[1] > -90 && yaxis.rot[1] < 90)  c("right", "centre")
                                                  else c("left", "centre"),
                                                  rot = yaxis.rot[1],
                                                  check.overlap = ylabelinfo$check.overlap,
                                                  gp =
                                                  gpar(col = yaxis.col.text,
                                                       fontfamily = yaxis.fontfamily,
                                                       fontface = chooseFace(yaxis.fontface, yaxis.font),
                                                       cex = yaxis.cex[1]),
                                                  vp = viewport(layout.pos.row = pos.row,
                                                  layout.pos.col = pos.col-4, yscale = yscale))

                            }


                            ## Y-axis to the right
                            if (column == cols.per.page) {

                                axstck <- x$y.scales$tck

                                ok <- seq(along = ylabelinfo$at)

                                pushViewport(viewport(layout.pos.row = pos.row,
                                                      layout.pos.col = pos.col+1,
                                                      yscale = yscale))


                                if (axstck[2] !=0 && any(ok))
                                    grid.segments(x0 = unit(rep(0, sum(ok)), "npc"),
                                                  x1 = unit(rep(0.3 * axstck[2], sum(ok)), "lines"),
                                                  y0 = unit(ylabelinfo$at[ok], "native"),
                                                  y1 = unit(ylabelinfo$at[ok], "native"),
                                                  gp =
                                                  gpar(col = yaxis.col.line,
                                                       lty = yaxis.lty,
                                                       lwd = yaxis.lwd))

                                upViewport()

                                if (y.alternating[actual.row]==2 || y.alternating[actual.row]==3)

                                    if (any(ok))

                                        grid.text(label = ylabelinfo$label[ok],
                                                  y = unit(ylabelinfo$at[ok], "native"),
                                                  x = unit(if (abs(yaxis.rot[2]) == 90) .5 else 0, "npc"),
                                                  ##y = unit(.05, "npc"),
                                                  just = if (yaxis.rot[2] == -90) c("centre", "centre")
                                                  else if (yaxis.rot[2] == 90) c("centre", "centre")
                                                  else if (yaxis.rot[2] > -90 && yaxis.rot[2] < 90)  c("left", "centre")
                                                  else c("right", "centre"),
                                                  rot = yaxis.rot[2],
                                                  check.overlap = ylabelinfo$check.overlap,
                                                  gp =
                                                  gpar(col = yaxis.col.text,
                                                       fontfamily = yaxis.fontfamily,
                                                       fontface = chooseFace(yaxis.fontface, yaxis.font),
                                                       cex = yaxis.cex[2]),
                                                  vp = viewport(layout.pos.row = pos.row,
                                                  layout.pos.col = pos.col+2, yscale = yscale))

                            }
                        }
                        
                        ## X-axis to the bottom
                        if (x.relation.same && x$x.scales$draw) {

                            if (actual.row == 1) {

                                axstck <- x$x.scales$tck

                                ok <- seq(along = xlabelinfo$at)

                                pushViewport(viewport(layout.pos.row = pos.row+3,
                                                      layout.pos.col = pos.col,
                                                      xscale = xscale))

                                if (axstck[1] !=0 && any(ok))
                                    grid.segments(y0 = unit(rep(1, sum(ok)), "npc"),
                                                  y1 = unit(rep(1, sum(ok)), "npc") -
                                                  unit(rep(0.3 * axstck[1], sum(ok)), "lines"),
                                                  x0 = unit(xlabelinfo$at[ok], "native"),
                                                  x1 = unit(xlabelinfo$at[ok], "native"),
                                                  gp =
                                                  gpar(col = xaxis.col.line,
                                                       lty = xaxis.lty,
                                                       lwd = xaxis.lwd))

                                upViewport()

                                if (x.alternating[column]==1 || x.alternating[column]==3) 

                                    if (any(ok)) {

                                        grid.text(label = xlabelinfo$lab[ok],
                                                  x = unit(xlabelinfo$at[ok], "native"),
                                                  y = unit(if (xaxis.rot[1] %in% c(0, 180)) .5 else 1, "npc"),
                                                  ##y = unit(rep(.95, sum(ok)), "npc"),
                                                  just = if (xaxis.rot[1] == 0) c("centre", "centre")
                                                  else if (xaxis.rot[1] == 180) c("centre", "centre")
                                                  else if (xaxis.rot[1] > 0)  c("right", "centre")
                                                  else c("left", "centre"),
                                                  rot = xaxis.rot[1],
                                                  check.overlap = xlabelinfo$check.overlap,
                                                  gp =
                                                  gpar(col = xaxis.col.text,
                                                       fontfamily = xaxis.fontfamily,
                                                       fontface = chooseFace(xaxis.fontface, xaxis.font),
                                                       cex = xaxis.cex[1]),
                                                  vp = viewport(layout.pos.row = pos.row + 4,
                                                  layout.pos.col = pos.col, xscale = xscale))
                                    }
                            }
                        }
                        
                        ##-------------------------
                        ## draw strip(s)

                        if (!is.logical(strip)) # logical <==> FALSE
                        {
                            ## which.panel in original cond variables order
                            which.panel = cond.current.level[x$perm.cond]

                            ## need to pass each index in original terms
                            for (i in seq(along = which.panel))
                                which.panel[i] <- x$index.cond[[i]][which.panel[i]]


                            ## WAS for(i in 1:number.of.cond)
                            for(i in seq(length = number.of.cond))
                            {
                                pushViewport(viewport(layout.pos.row = pos.row - i,
                                                      layout.pos.col = pos.col,
                                                      clip = trellis.par.get("clip")$strip))

## I have a choice here. By which.given, do I mean which in the original order,
## or the permuted order ? This is related to order in which strips are drawn
## (see above -- perm[i] or just i ?)

## currently, original                                
                                strip(which.given = x$perm.cond[i],
                                      which.panel = which.panel,

                                      var.name = names(x$condlevels),

                                      factor.levels = if (!is.list(x$condlevels[[x$perm.cond[i]]]))
                                      x$condlevels[[x$perm.cond[i]]] else NULL,

                                      shingle.intervals = if (is.list(x$condlevels[[x$perm.cond[i]]]))
                                      do.call("rbind", x$condlevels[[x$perm.cond[i]]]) else NULL,

                                      bg = strip.col.default.bg[i],
                                      fg = strip.col.default.fg[i],
                                      par.strip.text = par.strip.text)


## FIXME: add color for rectangle (already axis.line (?))
                                
                                grid.rect(gp =
                                          gpar(col = axis.line$col,
                                               lty = axis.line$lty,
                                               lwd = axis.line$lwd))

                                upViewport()
                                
                            }

                        }



                        
                        
                        ## X-axis at top
                        if (x.relation.same && x$x.scales$draw)

                            if (actual.row == rows.per.page) {

                                axstck <- x$x.scales$tck

                                ok <- seq(along = xlabelinfo$at)

                                pushViewport(viewport(layout.pos.row = pos.row - 1 - 
                                                      number.of.cond,
                                                      layout.pos.col = pos.col,
                                                      xscale = xscale))

                                if (axstck[2] !=0 && any(ok))
                                    grid.segments(y0 = unit(rep(0, sum(ok)), "npc"),
                                                  y1 = unit(rep(0.3 * axstck[2], sum(ok)), "lines"),
                                                  x0 = unit(xlabelinfo$at[ok], "native"),
                                                  x1 = unit(xlabelinfo$at[ok], "native"),
                                                  gp =
                                                  gpar(col = xaxis.col.line,
                                                       lty = xaxis.lty,
                                                       lwd = xaxis.lwd))

                                upViewport()

                                if (x.alternating[column]==2 || x.alternating[column]==3)

                                    if (any(ok))

                                        grid.text(label = xlabelinfo$label[ok],
                                                  x = unit(xlabelinfo$at[ok], "native"),
                                                  y = unit(if (xaxis.rot[2] %in% c(0, 180)) .5 else 0, "npc"),
                                                  ##y = unit(.05, "npc"),
                                                  just = if (xaxis.rot[2] == 0) c("centre", "centre")
                                                  else if (xaxis.rot[2] == 180) c("centre", "centre")
                                                  else if (xaxis.rot[2] > 0)  c("left", "centre")
                                                  else c("right", "centre"),
                                                  rot = xaxis.rot[2],
                                                  check.overlap = xlabelinfo$check.overlap,
                                                  gp =
                                                  gpar(col = xaxis.col.text,
                                                       fontfamily = xaxis.fontfamily,
                                                       fontface = chooseFace(xaxis.fontface, xaxis.font),
                                                       cex = xaxis.cex[2]),
                                                  vp = viewport(layout.pos.row = pos.row - 2 - 
                                                  number.of.cond, layout.pos.col = pos.col, xscale = xscale))


                            }

                        cond.current.level <- cupdate(cond.current.level,
                                                      cond.max.level)

                    }
                }
        


            ## legend / key plotting

            if (!is.null(legend))
            {
                locs <- names(legend)
                for (i in seq(along = legend))
                {
                    key.space <- locs[i]
                    key.gf <- legend[[i]]$obj

                    if (key.space == "left")
                    {
                        pushViewport(viewport(layout.pos.col = 2,
                                              layout.pos.row = c(6, n.row-6)))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "right")
                    {
                        pushViewport(viewport(layout.pos.col = n.col-1,
                                              layout.pos.row = c(6, n.row-6)))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "top")
                    {
                        pushViewport(viewport(layout.pos.row = 3,
                                              layout.pos.col = c(6,n.col-4)))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "bottom")
                    {
                        pushViewport(viewport(layout.pos.row = n.row - 2,
                                              layout.pos.col = c(6,n.col-4)))
                        grid.draw(key.gf)
                        upViewport()
                    }
                    else if (key.space == "inside")
                    {
                        pushViewport(viewport(layout.pos.row = c(1, n.row),
                                              layout.pos.col = c(1, n.col)))

                        key.corner <-
                            if (is.null(legend[[i]]$corner)) c(0,1)
                            else legend[[i]]$corner

                        key.x <- 
                            if (is.null(legend[[i]]$x)) key.corner[1]
                            else legend[[i]]$x

                        key.y <- 
                            if (is.null(legend[[i]]$y)) key.corner[2]
                            else legend[[i]]$y
                        
                        if (all(key.corner == c(0,1))) {
                            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                  widths = unit(c(key.x, 1, 1),
                                                  c("npc", "grobwidth", "null"),
                                                  list(1, key.gf, 1)),
                                                  heights = unit(c(1 - key.y, 1, 1),
                                                  c("npc", "grobheight", "null"),
                                                  list(1, key.gf, 1)))))
                        }
                        else if (all(key.corner == c(1,1))) {
                            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                  heights = unit(c(1 - key.y, 1, 1),
                                                  c("npc", "grobheight", "null"),
                                                  list(1, key.gf, 1)),
                                                  widths = unit(c(1, 1, 1 - key.x),
                                                  c("null", "grobwidth", "npc"),
                                                  list(1, key.gf, 1)))))
                        }
                        else if (all(key.corner == c(0,0))) {
                            pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                                  widths = unit(c(key.x, 1, 1),
                                                  c("npc", "grobwidth", "null"),
                                                  list(1, key.gf, 1)),
                                                  heights = unit(c(1, 1, key.y),
                                                  c("null", "grobheight", "npc"),
                                                  list(1, key.gf, 1)))))
                        }
                        else if (all(key.corner == c(1,0))) {
                            pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 3,
                                                  widths = unit(c(1, 1, 1 - key.x),
                                                  c("null", "grobwidth", "npc"),
                                                  list(1, key.gf, 1)),
                                                  heights = unit(c(1, 1, key.y),
                                                  c("null", "grobheight", "npc"),
                                                  list(1, key.gf, 1)))))
                        }
                        


                        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
                        grid.draw(key.gf)
                        upViewport(3)
                    }
                }
            }



            
            pushViewport(viewport(layout.pos.row = c(1, n.row),
                                  layout.pos.col = c(1, n.col)))
            if (!is.null(x$page)) x$page(page.number)                
            upViewport()
            
            upViewport()
            
            
        }
    }
    if (!missing(position)) {
        if (!missing(split)) {
            upViewport()
            upViewport()
        }
        upViewport()
    }
    else if (!missing(split)) {
        upViewport()
        upViewport()
    }

    if (!is.null(x$par.settings))
    {
        lset(opars)
    }

    invisible(x)
}
