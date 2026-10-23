

### Copyright (C) 2001-2006 Deepayan Sarkar
### <Deepayan.Sarkar@R-project.org>
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






paste.and.draw <-
    function(left, right, sep = " : ",
             horizontal = TRUE,
             center = TRUE,
             showl = TRUE,
             showr = TRUE,
             rot = if (horizontal) 0 else 90,
             type = if (horizontal) "strip.horizontal" else "strip.vertical",
             gp = gpar())
{
    ## We are already in a viewport.  Essentially want to draw
    ## paste(left, right, sep = sep) in the middle.  The catch is,
    ## left and right (maybe even sep) may be expressions.  The easy
    ## solution is to draw sep in the middle and left and right on
    ## either side.  The better solution is to combine and then
    ## center.

    ## NOTE: type = "

    if (showl || showr)
    {
        shows <- showl && showr
        wsep <- unit(0.5 * shows, "strwidth", list(sep))
        offset <- unit(0.5, "npc")
        if (center)
        {
            offset.adj <-
                (if (showl) unit(0.5, "strwidth", list(left)) else unit(0, "mm")) -
                    (if (showr) unit(0.5 * showr, "strwidth", list(right)) else unit(0, "mm"))
            if (!horizontal && rot < 0)
                offset <- offset - offset.adj
            else
                offset <- offset + offset.adj
        }
        if (horizontal)
        {
            if (shows) grid.text(sep, x = offset,
                                 name = trellis.grobname("sep", type = type),
                                 gp = gp)
            if (showl) grid.text(left, x = offset - wsep,
                                 name = trellis.grobname("textl", type = type),
                                 gp = gp, just = "right")
            if (showr) grid.text(right, x = offset + wsep,
                                 name = trellis.grobname("textr", type = type),
                                 gp = gp, just = "left")
        }
        else
        {
            if (rot < 0)
            {
                if (shows) grid.text(sep, y = offset,
                                     name = trellis.grobname("sep", type = type),
                                     gp = gp, rot = rot)
                if (showl) grid.text(left, y = offset + wsep,
                                     name = trellis.grobname("textl", type = type),
                                     gp = gp, just = "right", rot = rot)
                if (showr) grid.text(right, y = offset - wsep,
                                     name = trellis.grobname("textr", type = type),
                                     gp = gp, just = "left", rot = rot)
            }
            else
            {
                if (shows) grid.text(sep, y = offset,
                                     name = trellis.grobname("sep", type = type),
                                     gp = gp, rot = rot)
                if (showl) grid.text(left, y = offset - wsep,
                                     name = trellis.grobname("textl", type = type),
                                     gp = gp, just = "right", rot = rot)
                if (showr) grid.text(right, y = offset + wsep,
                                     name = trellis.grobname("textr", type = type),
                                     gp = gp, just = "left", rot = rot)
            }
        }
    }
}




strip.default <-
    function(which.given,
             which.panel,
##              packet.number,
##              panel.number,
             var.name,
             factor.levels,
             shingle.intervals = NULL,
             strip.names = c(FALSE, TRUE),
             strip.levels = c(TRUE, FALSE),
             sep = " : ",
             style = 1,
             horizontal = TRUE,
             side = if (horizontal) "top" else "left",
             ## FIXME: not sure how to incorporate alpha in strip colors
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"))
{
    if (!missing(side))
        horizontal <- side %in% c("top", "bottom")
    else
        side <- if (horizontal) "top" else "left"
    side <- match.arg(side, c("top", "left", "bottom", "right"))
    type <- if (side == "top") "strip" else paste("strip", side, sep = ".")
    rotDefault <- if (horizontal) 0 else if (side == "right") -90 else 90
    rot <- par.strip.text$rot %||% rotDefault
    vp.name <- paste(lattice.getStatus("current.prefix"),
                     paste(type, "default", sep = "."),
                     which.given, sep = ".")
    vp.name.off <- paste(lattice.getStatus("current.prefix"),
                         paste(type, "default.off", sep = "."),
                         which.given, sep = ".")
    n.panels <- length(which.panel)

    switch(side,
           top =
           pushViewport(viewport(y = (which.given - 0.5) / n.panels,
                                 height = 1 / n.panels,
                                 clip = trellis.par.get("clip")$strip,
                                 name = vp.name)),
           bottom =
           pushViewport(viewport(y = 1 - (which.given - 0.5) / n.panels,
                                 height = 1 / n.panels,
                                 clip = trellis.par.get("clip")$strip,
                                 name = vp.name)),
           left =
           pushViewport(viewport(x = 1 - (which.given - 0.5) / n.panels,
                                 width = 1 / n.panels,
                                 clip = trellis.par.get("clip")$strip,
                                 name = vp.name)),
           right =
           pushViewport(viewport(x = (which.given - 0.5) / n.panels,
                                 width = 1 / n.panels,
                                 clip = trellis.par.get("clip")$strip,
                                 name = vp.name)))

    gp.text <- 
        gpar(col = par.strip.text$col,
             alpha = par.strip.text$alpha,
             lineheight = par.strip.text$lineheight,
             fontfamily = par.strip.text$fontfamily,
             fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
             cex = par.strip.text$cex)

    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length.out = 2)
    strip.levels <- rep(strip.levels, length.out = 2)
    ## str(shingle.intervals)

    formatLabel <-
        function(s,
                 abbreviate = par.strip.text$abbr,
                 minlength = par.strip.text$minl,
                 dot = par.strip.text$dot)
    {
        if (is.null(abbreviate)) abbreviate <- FALSE
        if (is.null(minlength)) minlength <- 4
        if (is.null(dot)) dot <- FALSE
        if (abbreviate) abbreviate(s, minlength = minlength, dot = dot)
        else s
    }
    factor.levels <- formatLabel(factor.levels)

    if (!is.null(shingle.intervals))
    {

        ## This usually indicates shingles, as opposed to factors.
        ## 'style' will be completely ignored, and shingle.intervals
        ## encoded using bg and fg.  Names and levels are both game.

        grid.rect(name = trellis.grobname("bg", type = type),
                  gp = gpar(fill = bg, col = bg))

        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level,]) - t[1]) / diff(t)
        if (horizontal)
            grid.rect(x = unit(r %*% c(.5,.5),"npc"),
                      width = max(unit(c(diff(r), 1), c("npc", "mm"))),
                      name = trellis.grobname("fg", type = type),
                      gp = gpar(col = fg, fill = fg))
        else 
            grid.rect(y = unit(r %*% c(.5,.5),"npc"),
                      height = max(unit( c(diff(r), 1), c("npc", "mm"))),
                      name = trellis.grobname("fg", type = type),
                      gp = gpar(col = fg, fill = fg))

        paste.and.draw(name, factor.levels[level],
                       sep = sep,
                       horizontal = horizontal,
                       rot = rot,
                       type = type,
                       showl = strip.names[2],
                       showr = strip.levels[2],
                       gp = gp.text)
    }
    else
    {
        ## Behaviour depends on 'style'.  Will separate out coloring
        ## and text based on 'style'.

        num <- length(factor.levels)

        ## coloring:

        ## background: all except style = 2
        if (style != 2) {
            grid.rect(name = trellis.grobname("bg", type = type),
                      gp = gpar(fill = bg, col = bg))
        }

        ## foreground: needed only for style = 2, 3 and 4

        if (num > 0 && style %in% c(2, 3, 4))
        {
            if (horizontal)
            {
                grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                          width = unit(1/num, "npc"),
                          name = trellis.grobname("fg", type = type),
                          gp = gpar(fill = fg, col = fg))
            }
            else
            {
                y.fg <- if (rot < 0) 1 - (2*level-1)/(2*num) else (2*level-1)/(2*num)
                grid.rect(y = unit(y.fg, "npc"),
                          height = unit(1/num, "npc"),
                          name = trellis.grobname("fg", type = type),
                          gp = gpar(fill = fg, col = fg))
            }
        }

        ## text: [names|levels] centered only if style = 1 or 3

        if (style %in% c(1, 3))
        {
            paste.and.draw(name, factor.levels[level],
                           sep = sep,
                           horizontal = horizontal,
                           rot = rot,
                           type = type,
                           showl = strip.names[1],
                           showr = strip.levels[1],
                           gp = gp.text)
        }
        ## remaining cases
        else if (num > 0)
        {
            ## either all levels or only one
            lid <- if (style %in% c(2, 4)) 1:num else level
            if (horizontal)
            {
                grid.text(label = factor.levels[lid],
                          x = (2 * lid - 1) / (2 * num),
                          name = trellis.grobname("fg", type = type),
                          gp = gp.text)
            }
            else
            {
                y.text <- if (rot < 0) 1 - (2 * lid - 1) / (2 * num) else (2 * lid - 1) / (2 * num)
                grid.text(label = factor.levels[lid],
                          y = y.text,
                          rot = rot,
                          name = trellis.grobname("fg", type = type),
                          gp = gp.text)
            }
        }
    }
    upViewport()

    ## border is drawn with clipping off
    switch(side,
           top =
           pushViewport(viewport(y = (which.given - 0.5) / n.panels,
                                 height = 1 / n.panels,
                                 clip = "off",
                                 name = vp.name.off)),
           bottom =
           pushViewport(viewport(y = 1 - (which.given - 0.5) / n.panels,
                                 height = 1 / n.panels,
                                 clip = "off",
                                 name = vp.name.off)),
           left =
           pushViewport(viewport(x = 1 - (which.given - 0.5) / n.panels,
                                 width = 1 / n.panels,
                                 clip = "off",
                                 name = vp.name.off)),
           right =
           pushViewport(viewport(x = (which.given - 0.5) / n.panels,
                                 width = 1 / n.panels,
                                 clip = "off",
                                 name = vp.name.off)))


    strip.border <- trellis.par.get("strip.border")
    ## draw border for strip
    grid.rect(name = trellis.grobname("border", type = type),
              gp =
              gpar(col = rep(strip.border$col, length.out = which.given)[which.given],
                   lty = rep(strip.border$lty, length.out = which.given)[which.given],
                   lwd = rep(strip.border$lwd, length.out = which.given)[which.given],
                   alpha = rep(strip.border$alpha, length.out = which.given)[which.given],
                   fill = "transparent"))
    upViewport()
}





