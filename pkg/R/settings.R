

### Copyright 2001-2002 Deepayan Sarkar <deepayan@stat.wisc.edu>
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

col.whitebg <- function()
    list(background = list(col="transparent"),
         bar.fill = list(col="#c8ffc8"),
         box.rectangle = list(col="darkgreen"),
         box.umbrella = list(col="darkgreen"),
         dot.line = list(col="#e8e8e8"),
         dot.symbol = list(col="darkgreen"),
         plot.line = list(col="darkgreen"),
         plot.symbol = list(col="darkgreen"),
         ##regions=list(col=rev(hsv(h=250:349/1000, v=30:129/150,s=.5,
         ##gamma = .6)))
         regions = list(col = heat.colors(100)),
         strip.shingle = list(col = c("#ff7f00", "#00ff00", "#00ffff",
                                 "#0080ff", "#ff00ff", "#ff0000", "#ffff00")),
         strip.background = list(col = c("#ffe5cc", "#ccffcc", "#ccffff",
                                 "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")),
         reference.line = list(col="#e8e8e8"),
         superpose.line = list(col = c("darkgreen","red","royalblue",
                               "brown","orange","turquoise", "orchid"),
         lty = 1:7),
         superpose.symbol = list(pch = c(1,3,6,0,5,16,17), cex = rep(.7, 7),
         col = c("darkgreen","red","royalblue",
         "brown","orange","turquoise", "orchid")))


canonical.theme <- function(name = "null device", color = TRUE)
{
    ## For the purpose of this function, the only differences in the
    ## settings/themes arise from the difference in the default
    ## colors. So, I will first set up the appropriate colors
    ## according to 'name', and then use those to create the
    ## theme. The first 16 colors correspond to trellis.settings
    ## colors, the 17th is the background color.

    if (color)
    {
        ## color colors
        can.col <-
            if (name == "windows" || name == "X11")
                c("#000000", "#00ffff", "#ff00ff", "#00ff00",
                  "#ff7f00", "#007eff", "#ffff00", "#ff0000",
                  "#c6ffff", "#ffc3ff", "#c8ffc8", "#ffd18f",
                  "#a9e2ff", "#ffffc3", "#ff8c8a", "#aaaaaa",
                  "#909090")
            else if (name %in% c("postscript", "pdf", "xfig"))
                c("#000000", "#00ffff", "#ff00ff", "#00ff00",
                  "#ff7f00", "#0080ff", "#ffff00", "#ff0000",
                  "#ccffff", "#ffccff", "#ccffcc", "#ffe5cc",
                  "#cce6ff", "#ffffcc", "#ffcccc", "#e6e6e6",
                  "transparent")
            else ## default, same as X11 for now
                c("#000000", "#00FFFF", "#FF00FF", "#00FF00",
                  "#FF7F00", "#007EFF", "#FFFF00", "#FF0000",
                  "#C6FFFF", "#FFC3FF", "#C8FFC8", "#FFD18F",
                  "#A9E2FF", "#FFFFC3", "#FF8C8A", "#AAAAAA",
                  "#909090")
    }
    else ## b&w colors, same for all devices (8:16 actually unnecessary)
        can.col <-
            c("#000000", "#999999", "#4C4C4C", "#E6E6E6", "#F2F2F2",
              "#B2B2B2", "#000000", "#030303", "#050505", "#080808",
              "#0A0A0A", "#0D0D0D", "#0F0F0F", "#121212", "#151515",
              "#171717", "transparent")

    ## The following definition is the basis for what elements are
    ## valid in any setting. Adding something here should be necessary
    ## and sufficient.

    ## color settings, modified later if postscript or color = FALSE
    ans <-
        list(fontsize         = list(text = 12, points = 8),
             background       = list(alpha = 1, col = can.col[17]),
             clip             = list(panel = "on", strip = "on"),
             add.line         = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1),
             add.text         = list(alpha = 1, cex = 1, col = can.col[1], font = 1),
             bar.fill         = list(alpha = 1, col = can.col[2], border = "black", lty = 1, lwd = 1),
             box.dot          = list(alpha = 1, col = can.col[1], cex = 1, font = 1, pch = 16),
             box.rectangle    = list(alpha = 1, col = can.col[2], fill = "transparent", lty = 1, lwd = 1),
             box.umbrella     = list(alpha = 1, col = can.col[2], lty = 2, lwd = 1),
             dot.line         = list(alpha = 1, col = can.col[16], lty = 1, lwd = 1),
             dot.symbol       = list(alpha = 1, cex = 0.8, col = can.col[2], font = 1, pch = 16),
             plot.line        = list(alpha = 1, col = can.col[2], lty = 1, lwd = 1),
             plot.symbol      = list(alpha = 1, cex = 0.8, col = can.col[2], font = 1, pch = 1),
             reference.line   = list(alpha = 1, col = can.col[16], lty = 1, lwd = 1),
             strip.background = list(alpha = 1, col = can.col[c(12, 11, 9, 13, 10, 15, 14)]),
             strip.shingle    = list(alpha = 1, col = can.col[c(5, 4, 2, 6, 3, 8, 7)]),
             strip.border     = list(alpha = 1, col = rep(can.col[1], 7), lty = rep(1, 7), lwd = rep(1, 7)),
             superpose.line   = list(alpha = 1, col = can.col[2:8], lty = rep(1, 7), lwd = rep(1, 7)),
             superpose.symbol = list(alpha = rep(1, 7), cex = rep(0.8, 7), col = can.col[2:8],
                                     font = rep(1, 7), pch = rep(1, 7)),
             superpose.fill   = list(alpha = rep(1, 7), col = can.col[2:8],
                                     border = rep("black", 7), lty = rep(1, 7), lwd = rep(1, 7)),
             regions          = list(alpha = 1, col = rev(cm.colors(100))),
             shade.colors     = list(alpha = 1, palette = function(irr, ref, height, saturation = .9) {
                 hsv(h = height, s = 1 - saturation * (1 - (1-ref)^0.5), v = irr)
             }),
             axis.line        = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1),
             axis.text        = list(alpha = 1, cex = .8, col = can.col[1], font = 1),

             ## NEW: controls widths of tick marks and padding of labels
             axis.components  = list(left = list(tck = 1, pad1 = 1, pad2 = 1),
                                     top = list(tck = 1, pad1 = 1, pad2 = 1),
                                     right = list(tck = 1, pad1 = 1, pad2 = 1),
                                     bottom = list(tck = 1, pad1 = 1, pad2 = 1)),
             ## NEW: controls widths of basic layout's components
             layout.heights   = list(top.padding = 1,
                                     main = 1,
                                     main.key.padding = 1,
                                     key.top = 1,
                                     key.axis.padding = 1,
                                     axis.top = 1,
                                     strip = 1,
                                     panel = 1, ## shouldn't be changed
                                     axis.panel = 1,
                                     between = 1,
                                     axis.bottom = 1,
                                     axis.xlab.padding = 1,
                                     xlab = 1,
                                     xlab.key.padding = 1,
                                     key.bottom = 1,
                                     key.sub.padding = 1,
                                     sub = 1,
                                     bottom.padding = 1),
             layout.widths    = list(left.padding = 1,
                                     key.left = 1,
                                     key.ylab.padding = 1,
                                     ylab = 1,
                                     ylab.axis.padding = 1,
                                     axis.left = 1,
                                     axis.panel = 1,
                                     panel = 1, ## shouldn't be changed
                                     between = 1,
                                     axis.right = 1,
                                     axis.key.padding = 1,
                                     key.right = 1,
                                     right.padding = 1),

             box.3d           = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1),
             par.xlab.text    = list(alpha = 1, cex = 1, col = can.col[1], font = 1),
             par.ylab.text    = list(alpha = 1, cex = 1, col = can.col[1], font = 1),
             par.zlab.text    = list(alpha = 1, cex = 1, col = can.col[1], font = 1),
             par.main.text    = list(alpha = 1, cex = 1.2, col = can.col[1], font = 2),
             par.sub.text     = list(alpha = 1, cex = 1, col = can.col[1], font = 2))

    if (color)
    {
        if (name == "postscript" || name == "pdf")
        {
            ans$plot.symbol$col <- can.col[6]
            ans$plot.line$col <- can.col[6]
            ans$dot.symbol$col <- can.col[6]
            ans$box.rectangle$col <- can.col[6]
            ans$box.umbrella$col <- can.col[6]
            ans$superpose.symbol$col <- c(can.col[c(6, 3, 4, 8)],
                                          "orange", "darkgreen", "brown")
            ans$superpose.line$col <- c(can.col[c(6, 3, 4, 8)],
                                          "orange", "darkgreen", "brown")
        }
    }
    else {
        ## black and white settings
        ans$bar.fill$col <- can.col[5]
        ans$box.dot$col <- can.col[1]
        ans$box.rectangle$col <- can.col[1]
        ans$box.umbrella$col <- can.col[1]
        ans$box.umbrella$lty <- 2
        ans$dot.line$col <- can.col[4]
        ans$dot.symbol$col <- can.col[1]
        ans$dot.symbol$cex <- 0.85
        ans$plot.line$col <- can.col[1]
        ans$plot.symbol$col <- can.col[1]
        ans$regions$col <- gray(29:128/128)
        ans$shade.colors$palette <-
            function(irr, ref, height, w = .5)
                grey(w * irr + (1 - w) * (1 - (1-ref)^.4))
        ans$reference.line$col <- can.col[4]
        ans$strip.background$col <- can.col[rep(5, 7)]
        ans$strip.shingle$col <- can.col[rep(6, 7)]
        ans$superpose.line$col <- can.col[rep(1, 7)]
        ans$superpose.line$lty <- 1:7
        ans$superpose.symbol$col <- can.col[rep(1, 7)]
        ans$superpose.symbol$cex <- rep(0.7, 7)
        ans$superpose.symbol$pch <- c(1,3,6,0,5,16,17)
        ans$superpose.fill$col <- gray( (c(6, 12, 7, 11, 8, 10, 9)/15)^.8 )
        ##ans$superpose.symbol$pch <- c("o","+",">","s","w","#","{")
    }
    ans
}   





trellis.par.get <-
    function(name = NULL)
{
    ## the default device is opened if none already open
    if (is.null(dev.list())) trellis.device()
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)

    ## just in case settings for the current device haven't been
    ## created yet, which may happen if the device is opened by x11(),
    ## say, (i.e., not by trellis.device()) and no trellis object has
    ## been printed on this device yet. 

    if (is.null(lattice.theme[[.Device]])) {
        trellis.device(device = .Device, new = FALSE)
        lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    }
    if (is.null(name))
        lattice.theme[[.Device]]
    else if (name %in% names(lattice.theme[[.Device]]))
        lattice.theme[[.Device]][[name]]
    else NULL
}



trellis.par.set <-
    function(name, value, ..., theme, warn = TRUE)
{
    ## the default device is opened if none already open
    if (is.null(dev.list())) {
        trellis.device()
        if (warn)
            cat("Note: The default device has been opened to honour attempt to modify trellis settings\n",
                fill = TRUE)
    }





    ## if (name %in% names(lattice.theme[[.Device]])) NEEDED as a safeguard ?
    ## if (!is.list(value)) stop("value must be a list")
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    ## make sure a list for this device is present
    if (is.null(lattice.theme[[.Device]])) trellis.device(device = .Device, new = FALSE)
    ## WAS: lattice.theme[[.Device]][[name]] <- value

    if (missing(theme))
        if (!missing(value))
        {
            theme <- list(value)
            names(theme) <- name
        }
        else if (!missing(name) && is.list(name))
        {
            theme <- name
        }
        else theme <- list(...)

    lattice.theme[[.Device]] <- updateList(lattice.theme[[.Device]], theme)
    assign("lattice.theme", lattice.theme, envir=.LatticeEnv)
    invisible()
}



trellis.device <-
    function(device = getOption("device"),
             color = !(dev.name == "postscript"),
             theme = lattice.getOption("default.theme"),
##             bg = NULL,
             new = TRUE,
             retain = FALSE,
             ...)
{
    ## Get device function
    if (is.character(device))
    {
        if (new || is.null(dev.list()))
        {   # to make sure this works even if package grDevices is not loaded
            device.call <- try(get(device), silent = TRUE)
            if (inherits(device.call, "try-error"))
                device.call <-
                    try(utils::getFromNamespace(device, "grDevices"),
                        silent = TRUE)
            if (inherits(device.call, "try-error"))
                stop(paste("Could not find device function", device))
        }
        dev.name <- device
    }
    else
    {
        device.call <- device
        dev.name <- deparse(substitute(device))
    }

    ## Start the new device if necessary.
    ## new = FALSE ignored if no devices open.


    ## FIXME: remove this warning in some future version
    if ("bg" %in% names(list(...)))
        warning("trellis.device has changed, 'bg' may not be doing what you think it is")

    if (new || is.null(dev.list()))
    {
        device.call(...)
        lattice.setStatus(print.more = FALSE)
    }

    ## Make sure there's an entry for this device in the theme list
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    if (!(.Device %in% names(lattice.theme)))
    {
        lattice.theme[[.Device]] <- canonical.theme(name = .Device, color = color)
        assign("lattice.theme", lattice.theme, envir = .LatticeEnv)
    }

    ## If retain = FALSE, overwrite with default settings for device
    if (!retain) trellis.par.set(canonical.theme(name=.Device, color=color))

    ## get theme as list
    if (!is.null(theme) && !is.list(theme))
    {
        if (is.character(theme)) theme <- get(theme)
        if (is.function(theme)) theme <- theme()
        if (!is.list(theme))
        {
            warning("Invalid theme specified")
            theme <- NULL
        }
    }

    ## apply theme 
    if (!is.null(theme)) trellis.par.set(theme)
##    if (!is.null(bg)) trellis.par.set(list(background = list(col = bg)))
    return(invisible())
}



lset <- function(theme = col.whitebg())
{
    .Deprecated("trellis.par.set")
    trellis.par.set(theme = theme)
#     for (item in names(theme))
#     {
#         foo <- trellis.par.get(item)
#         bar <- theme[[item]]
#         foo[names(bar)] <- bar
#         trellis.par.set(item, foo)
#     }
}







show.settings <- function(x = NULL)
{
    if (is.null(dev.list())) trellis.device()
    theme <- trellis.par.get()
    if (is.null(theme) && is.null(x)) print("No active device") ## shouldn't happen
    else {
        if (is.null(theme)) { ## also shouldn't happen
            cat("\nNo device is currently active but a theme has been explicitly specified.")
            cat("\nDefault device options will be used to fill missing components")
            cat("\nof specified theme, if any.\n")
            theme <- canonical.theme(getOption("device"))
        }
        if (!is.null(x)) {
            for (item in names(x)) {
                foo <- x[[item]]
                theme[[item]][names(foo)] <- foo
            }
        }
        n.row <- 13
        n.col <- 9
        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.units[c(2, 5, 8, 11)] <- "null"
        widths.x <- rep(1, n.row)
        widths.units <- rep("lines", n.row)
        widths.units[c(2, 4, 6, 8)] <- "null"
        page.layout <- grid.layout(nrow = n.row, ncol = n.col,
                                   widths = unit(widths.x, widths.units),
                                   heights = unit(heights.x, heights.units))
        if (!lattice.getStatus("print.more")) grid.newpage()
        lattice.setStatus(print.more = FALSE)
        grid.rect(gp = gpar(fill = theme$background$col,
                  col = "transparent"))
        pushViewport(viewport(layout = page.layout,
                              gp = gpar(fontsize = theme$fontsize$text)))
        superpose.symbol <- theme$superpose.symbol
        len <- length(superpose.symbol$col)
        pushViewport(viewport(layout.pos.row = 2,
                              layout.pos.col = 2,
                              yscale = c(0,len+1),
                              xscale = c(0,len+1)))
        for (i in 1:len)
        {
            lpoints(y = rep(i, len), x = 1:len,
                    col = superpose.symbol$col[i],
                    cex = superpose.symbol$cex[i],
                    font = superpose.symbol$font[i],
                    fontfamily = superpose.symbol$fontfamily[i],
                    fontface = superpose.symbol$fontface[i],
                    pch = superpose.symbol$pch[i])
        }
        popViewport()
        grid.text(lab = "superpose.symbol",
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
        superpose.line <- theme$superpose.line
        len <- length(superpose.line$col)
        pushViewport(viewport(layout.pos.row = 2,
                               layout.pos.col = 4,
                               yscale = c(0,len+1),
                               xscale = c(0,1)))
        for (i in 1:len) {
            llines(y = rep(i, 2), x = c(0,1),
                   col = superpose.line$col[i],
                   lty = superpose.line$lty[i],
                   lwd = superpose.line$lwd[i])
        }
        popViewport()
        grid.text(lab = "superpose.line",
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 4))
        strip.background <- theme$strip.background
        len <- length(strip.background$col)
        pushViewport(viewport(layout.pos.row = 2,
                               layout.pos.col = 6,
                               yscale = c(0,len+1),
                               xscale = c(0,1)))
        for (i in 1:len) {
            grid.rect(y = unit(i, "native"), h = unit(.5, "native"),
                      gp = gpar(fill = strip.background$col[i]))
        }
        popViewport()
        grid.text(lab = "strip.background",
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 6))
        strip.shingle <- theme$strip.shingle
        len <- length(strip.shingle$col)
        pushViewport(viewport(layout.pos.row = 2,
                               layout.pos.col = 8,
                               yscale = c(0,len+1),
                               xscale = c(0,1)))
        for (i in 1:len) {
            grid.rect(y = unit(i, "native"), h = unit(.5, "native"),
                      gp = gpar(fill = strip.shingle$col[i]))
        }
        popViewport()
        grid.text(lab = "strip.shingle",
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 8))
        pushViewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 2,
                               yscale = extend.limits(c(0,6)),
                               xscale = c(0,6)))
        x <- c(1,2,3,4,5)
        dot.line <- theme$dot.line
        dot.symbol <- theme$dot.symbol
        panel.abline(h=1:5, col=dot.line$col,
                     lty=dot.line$lty, lwd=dot.line$lwd)
        panel.xyplot(x = x, y = x, col = dot.symbol$col, pch = dot.symbol$pch)
        grid.rect()
        popViewport()
        grid.text(lab = "dot.[symbol, line]",
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 2))
        box.rectangle <- theme$box.rectangle
        box.dot <- theme$box.dot
        box.umbrella <- theme$box.umbrella
        pushViewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 4,
                               yscale = c(-1.5,1.5),
                               xscale = c(0,6)))
        pushViewport(viewport(height = unit(.1, "npc")))
        grid.rect(width = 1/3, 
                  gp = gpar(lwd = box.rectangle$lwd, 
                  lty = box.rectangle$lty,
                  fill = box.rectangle$fill,
                  col = box.rectangle$col))
        grid.lines(x = unit(c(1/6, 1/3), "npc"), 
                   y = unit(c(0.5, 0.5), "npc"),
                   gp = gpar(col = box.umbrella$col,
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.lines(x = unit(c(2/3, 5/6), "npc"), 
                   y = unit(c(0.5, 0.5), "npc"),
                   gp = gpar(col = box.umbrella$col,
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.lines(x = unit(rep(1/6, 2), "npc"), 
                   y = unit(c(0, 1), "npc"),
                   gp = gpar(col = box.umbrella$col, 
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.lines(x = unit(rep(5/6, 2), "npc"), 
                   y = unit(c(0, 1), "npc"),
                   gp = gpar(col = box.umbrella$col, 
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.points(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                    pch = box.dot$pch,
                    gp =
                    gpar(col = box.dot$col,
                         fontsize = theme$fontsize$points,
                         fontfamily = box.dot$fontfamily,
                         fontface = chooseFace(box.dot$fontface, box.dot$font),
                         cex = box.dot$cex))

        popViewport()
        grid.rect()
        popViewport()
        grid.text(lab = "box.[dot, rectangle, umbrella]",
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 4))
        add.text <- theme$add.text
        add.line <- theme$add.line
        pushViewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 6,
                               yscale = c(-1,1),
                               xscale = c(0,1)))
        x <- seq(.1, .9, length = 50)
        y <- .9 * sin(.1+11*x)
        llines(x = x, y = y, type = "l", col = add.line$col,
               lty = add.line$lty, lwd = add.line$lwd)
        grid.text(lab = c("Hello", "World"),
                  x = c(.25, .75), y = c(-.5, .5), default.units = "native",
                  gp =
                  gpar(col = add.text$col,
                       cex = add.text$cex,
                       fontfamily = add.text$fontfamily,
                       fontface = chooseFace(add.text$fontface, add.text$font)))
        grid.rect()
        popViewport()
        grid.text(lab = "add.[line, text]",
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 6))
        reference.line <- theme$reference.line
        pushViewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 8,
                               yscale = c(0,4),
                               xscale = c(0,4)))
        panel.grid(col = reference.line$col,
                   lwd = reference.line$lwd,
                   lty = reference.line$lty)
        grid.rect()
        popViewport()
        grid.text(lab = "reference.line",
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 8))
        plot.symbol <- theme$plot.symbol
        plot.line <- theme$plot.line
        pushViewport(viewport(layout.pos.row = 8,
                               layout.pos.col = 2,
                               yscale = c(-1.1,1.1),
                               xscale = c(-.1,1.1)))
        x <- seq(.1, .9, length = 20)
        y <- .9 * sin(.1+11*x)
        panel.xyplot(x = x+.05, y = y+.1, type = "l",
                     lty = plot.line$lty,
                     col = plot.line$col,
                     lwd = plot.line$lwd)
        panel.xyplot(x = x-.05, y = y-.1,
                     col = plot.symbol$col,
                     fontfamily = plot.symbol$fontfamily,
                     fontface = chooseFace(plot.symbol$fontface, plot.symbol$font), 
                     pch = plot.symbol$pch,
                     cex = plot.symbol$cex)
        grid.rect()
        popViewport()
        grid.text(lab = "plot.[symbol, line]",
                  vp = viewport(layout.pos.row = 9, layout.pos.col = 2))
        bar.fill <- theme$bar.fill
        pushViewport(viewport(layout.pos.row = 8,
                               layout.pos.col = 4,
                               yscale = extend.limits(c(0,6)),
                               xscale = extend.limits(c(1,10))))
        grid.rect(x = c(3.5, 4.5, 5.5, 6.5, 7.5), w = rep(5,5),
                  y = c(1,2,3,4,5), h = rep(.5, ,5),
                  default.units = "native",
                  gp = gpar(fill = bar.fill$col))
        grid.rect()
        popViewport()
        grid.text(lab = "plot.shingle[bar.fill]",
                  vp = viewport(layout.pos.row = 9, layout.pos.col = 4))
        pushViewport(viewport(layout.pos.row = 8,
                               layout.pos.col = 6,
                               yscale = extend.limits(c(0,7)),
                               xscale = extend.limits(c(0,7))))
        grid.rect(y = c(.5, 1, 1.5, 2, 2.5, 3, 3.5), w = rep(1,7),
                  x = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), h = 1:7,
                  default.units = "native",
                  gp = gpar(fill = bar.fill$col))
        grid.rect()
        popViewport()
        grid.text(lab = "histogram[bar.fill]",
                  vp = viewport(layout.pos.row = 9, layout.pos.col = 6))
        pushViewport(viewport(layout.pos.row = 8,
                               layout.pos.col = 8,
                               yscale = extend.limits(c(0,6)),
                               xscale = c(0,7)))
        grid.rect(x = rev(c(.5, 1, 1.5, 2, 2.5, 3)), h = rep(.5, 6),
                  y = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), w = 6:1,
                  default.units = "native",
                  gp = gpar(fill = bar.fill$col))
        grid.rect()
        popViewport()
        grid.text(lab = "barchart[bar.fill]",
                  vp = viewport(layout.pos.row = 9, layout.pos.col = 8))
        regions <- theme$regions
        len <- length(regions$col)
        pushViewport(viewport(layout.pos.row = 11,
                               layout.pos.col = 2,
                               xscale = c(0,len+1)))
        for (i in 1:len)
            grid.rect(x = i, w = 1, default.units = "native",
                      gp = gpar(col = NULL,  fill = regions$col[i]))
        grid.rect()
        popViewport()
        grid.text(lab = "regions",
                  vp = viewport(layout.pos.row = 12, layout.pos.col = 2))
    }    
    invisible()
}









## non-graphical options

lattice.getOption <- function(name)
{
    get("lattice.options", envir = .LatticeEnv)[[name]]
}


## FIXME: lattice.options(foo == 1) doesn't work?
lattice.options <- function(...)
{
    ## this would have been really simple if only form allowed were
    ## lattice.options("foo", "bar") and
    ## lattice.options(foo=1, bar=2). But it could also be
    ## lattice.options(foo=1, "bar"), which makes some juggling necessary
    
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .LatticeEnv$lattice.options
    ## any reason to prefer get("lattice.options", envir = .LatticeEnv)?

    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old) 

    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)]) ## typically getting opions, not setting
    isNamed <- nm != "" ## typically all named when setting, but could have mix
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])
    ## so now everything has non-"" names, but only the isNamed ones should be set
    ## everything should be returned, however

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    modified <- updateList(retVal[nm], new[nm])
    .LatticeEnv$lattice.options[names(modified)] <- modified
    invisible(retVal)
}



.defaultLatticeOptions <- function()
    list(save.object = TRUE,
         drop.unused.levels = list(cond = TRUE, data = TRUE),
         default.theme = getOption("lattice.theme"), ## for back compatibility, usually NULL
         axis.units =
         list(outer =
              list(left =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   top =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   right =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   bottom =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm"))),

              inner =
              list(left =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   top =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   right =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   bottom =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")))),


         layout.heights =
         list(top.padding = list(x = 2, units = "mm", data = NULL),
              main = list(x = 0, units = "grobheight", data = textGrob(lab="")),
              main.key.padding = list(x = 2, units = "mm", data = NULL),
              key.top = list(x = 0, units = "grobheight", data = textGrob(lab="")),
              key.axis.padding = list(x = 2, units = "mm", data = NULL),
              axis.top = list(x = 0, units = "mm", data = NULL),
              strip = list(x = 1, units = "lines", data = NULL),
              panel = list(x = 1, units = "null", data = NULL),
              axis.panel = list(x = 0, units = "mm", data = NULL),
              between = list(x = 5, units = "mm", data = NULL),
              axis.bottom = list(x = 0, units = "mm", data = NULL),
              axis.xlab.padding = list(x = 2, units = "mm", data = NULL),
              xlab = list(x = 0, units = "grobheight", data = textGrob(lab="")),
              xlab.key.padding = list(x = 2, units = "mm", data = NULL),
              key.bottom = list(x = 0, units = "grobheight", data = textGrob(lab="")),
              key.sub.padding = list(x = 2, units = "mm", data = NULL),
              sub = list(x = 0, units = "grobheight", data = textGrob(lab="")),
              bottom.padding = list(x = 2, units = "mm", data = NULL)),
         layout.widths =
         list(left.padding = list(x = 2, units = "mm", data = NULL),
              key.left = list(x = 0, units = "grobwidth", data = textGrob(lab="")),
              key.ylab.padding = list(x = 2, units = "mm", data = NULL),
              ylab = list(x = 0, units = "grobheight", data = textGrob(lab="")),
              ylab.axis.padding = list(x = 2, units = "mm", data = NULL),
              axis.left = list(x = 0, units = "mm", data = NULL),
              axis.panel = list(x = 0, units = "mm", data = NULL),
              panel = list(x = 1, units = "null", data = NULL),
              between = list(x = 5, units = "mm", data = NULL),
              axis.right = list(x = 0, units = "mm", data = NULL),
              axis.key.padding = list(x = 2, units = "mm", data = NULL),
              key.right = list(x = 0, units = "grobwidth", data = textGrob(lab="")),
              right.padding = list(x = 2, units = "mm", data = NULL)),

         highlight.gpar = list(col = "red", lwd = 2)

         )






lattice.getStatus <- function(name)
    get("lattice.status", envir = .LatticeEnv)[[name]]

lattice.setStatus <- function (...) 
{
    dots <- list(...)
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
        dots <- dots[[1]]
    if (length(dots) == 0) 
        return()
    lattice.status <- get("lattice.status", envir = .LatticeEnv)
    lattice.status[names(dots)] <- dots
    assign("lattice.status", lattice.status, env = .LatticeEnv)
}
    



.defaultLatticeStatus <- function()
    list(print.more = FALSE,
         current.plot.saved = FALSE,
         current.plot.multipage = FALSE,
         current.focus.row = 0,
         current.focus.column = 0,
         vp.highlighted = FALSE)

