
xyplot.ts <-
    function(x, data = NULL,
             screens = if (superpose) 1 else colnames(x),
             ...,
             superpose = FALSE, ## not used directly
             cut = FALSE,
             type = "l",
             col = if (!superpose) plot.line$col,
             lty = if (!superpose) plot.line$lty,
             lwd = if (!superpose) plot.line$lwd,
             pch = if (!superpose) plot.symbol$pch,
             auto.key = superpose,
             par.settings = list(),
             layout = NULL, as.table = TRUE,
             xlab = "Time", ylab = NULL,
             default.scales = list(y = list(relation =
                 if (missing(cut)) "free" else "same")))
{
    plot.line <- trellis.par.get("plot.line")
    plot.symbol <- trellis.par.get("plot.symbol")
    stopifnot(is.null(data))
    timex <- time(x)
    x <- as.matrix(x)
    if (is.null(colnames(x)))
        colnames(x) <- paste("V", seq_len(NCOL(x)), sep = "")
    cn <- colnames(x)

    ## set up shingle for cut-and-stack plot
    ## (may not work well with irregular series)
    time <- NULL
    if (is.numeric(cut)) cut <- list(number = cut)
    if (isTRUE(cut)) {
        ## calculate optimum aspect ratio using banking (as for aspect = "xy")
        timediff <- diff(timex)
        asp <- apply(x, 2, function(y)
                     banking(timediff, diff(y)) * diff(range(y)) / diff(range(timex))
                     )
        asp <- median(asp)
        ## work out aspect ratio of n panels in vertical layout on a square
        nasp <- 1 / (1:6)
        ## choose number of cuts so that the "xy" aspect matches layout
        number <- which.min(abs(1 - (asp * 1:6) / nasp))
        cut <- list(number = number)
        if (number == 1) cut <- FALSE
    }
    if (is.list(cut))
    {
        ecargs <- list(x = timex)
        ecargs <- modifyList(ecargs, cut)
        time <- do.call(equal.count, ecargs)
        default.scales <-
            modifyList(list(x = list(relation = "sliced")),
                       default.scales)
    }

    ## 'screens' defines which panels to draw series in
    ## if 'screens' was given as a readable value then always show strips
    screensgiven <- !missing(screens) && !is.numeric(screens)
    screens <- make.par.list(cn, screens, NROW(x), NCOL(x), 1)
    screens <- unlist(screens, use.names = FALSE)
    screens <- factor(screens, levels = unique(screens))
    screens <- rep(screens, length = NCOL(x))
    fac <- factor(rep(screens, each = NROW(x)))

    ## formula
    tt <- rep(timex, NCOL(x))
    fo <- if ((nlevels(fac) > 1) || screensgiven) {
        if (!is.null(time))
            x ~ tt | time * fac
        else
            x ~ tt | fac
    } else {
        if (!is.null(time))
            x ~ tt | time
        else
            x ~ tt
    }

    if (is.null(layout)) {
        npanels <- max(1, nlevels(fac)) * max(1, nlevels(time))
        nc <- ceiling(npanels/6)
        nr <- ceiling(npanels/nc)
        layout <- c(nc, nr)
    }

    ## set lines, not points, as default for key
    if (is.logical(auto.key) && auto.key) auto.key <- list()
    if (is.list(auto.key))
        auto.key <-
            modifyList(list(lines = TRUE, points = FALSE), auto.key)

    ## update with style arguments (col, lty, etc) only if specified.
    if (!is.null(col)) {
        col <- unlist(make.par.list(cn, col, NROW(x), NCOL(x), plot.line$col))
        par.settings <- modifyList(simpleTheme(col = col), par.settings)
    }
    if (!is.null(lty)) {
        lty <- unlist(make.par.list(cn, lty, NROW(x), NCOL(x), plot.line$lty))
        par.settings <- modifyList(simpleTheme(lty = lty), par.settings)
    }
    if (!is.null(lwd)) {
        lwd <- unlist(make.par.list(cn, lwd, NROW(x), NCOL(x), plot.line$lwd))
        par.settings <- modifyList(simpleTheme(lwd = lwd), par.settings)
    }
    if (!is.null(pch)) {
        pch <- unlist(make.par.list(cn, pch, NROW(x), NCOL(x), plot.symbol$pch))
        par.settings <- modifyList(simpleTheme(pch = pch), par.settings)
    }
    if (is.list(type))
        type <- make.par.list(cn, type, NROW(x), NCOL(x), "l")

    obj <-
        xyplot(fo, groups = factor(col(x), labels = cn),
               ...,
               type = type, distribute.type = is.list(type),
               auto.key = auto.key, par.settings = par.settings,
               layout = layout, as.table = as.table,
               xlab = xlab, ylab = ylab,
               default.scales = default.scales)

    obj$call <- sys.call(sys.parent()); obj$call[[1]] <- quote(xyplot)
    obj
}

## COPIED FROM ZOO
## http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/zoo/R/plot.zoo.R?rev=609&root=zoo&view=markup

make.par.list <- function(nams, x, n, m, def, recycle = sum(unnamed) > 0) {
    ## if nams are the names of our variables and x is a parameter
    ## specification such as list(a = c(1,2), c(3,4)) then
    ## create a new list which uses the named variables from x
    ## and assigns the unnamed in order.  For the remaining variables
    ## assign them the default value if recycle = FALSE or recycle the
    ## unnamed variables if recycle = TRUE.  The default value for
    ## recycle is TRUE if there is at least one unnamed variable
    ## in x and is false if there are only named variables in x.
    ## n is the length of the series and m is the total number of series
    ## It only needs to know whether m is 1 or greater than m.
    ## def is the default value used when recycle = FALSE
    ## recycle = TRUE means recycle unspecified values
    ## recycle = FALSE means replace values for unspecified series with def
    ## Within series recycling is done even if recycle=FALSE.

    ## Should we allow arbirary names in 1d case?
    ## if (m > 1) stopifnot(all(names(x) %in% c("", nams)))
    if (!is.list(x)) x <- if (m == 1) list(x) else as.list(x)
    y <- vector(mode = "list", length = length(nams))
    names(y) <- nams
    in.x <- nams %in% names(x)
    unnamed <- if (is.null(names(x))) rep(TRUE, length(x)) else names(x) == ""
    if (!recycle) y[] <- def
    y[in.x] <- x[nams[in.x]]
    if (recycle) {
        stopifnot(sum(unnamed) > 0)
        y[!in.x] <- rep(x[unnamed], length.out = sum(!in.x)) ## CHECK, this was: x[unnamed]
    } else {
        y[which(!in.x)[seq_len(sum(unnamed))]] <- x[unnamed]
    }
    lapply(y, function(y) if (length(y)==1) y else rep(y, length.out = n))
}
