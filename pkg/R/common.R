### Copyright (C) 2001-2005  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
### Copyright (C) 2001-2005  Saikat DebRoy <saikat@stat.wisc.edu>
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




cupdate <- function(index, maxim)
{

    ## This unexported function is used to handle arbitrary number of
    ## conditioning variables : every time it is called, it increments
    ## the "current" level of the conditioning variables suitably,
    ## i.e., it tries to increment the level of the 1st conditining
    ## variable (the one which varies fastest along panel order) and
    ## if it happens to be at its maximum (last) value, it sets it to
    ## the first value AND increments the "current" level of the 2nd
    ## (next) conditioning variable recursively.

    if(length(index)!=length(maxim)||length(maxim)<=0)
        stop("Inappropriate arguments")
    index[1] <- index[1] + 1
    if(index[1]>maxim[1] && length(maxim)>1)
        c(1,cupdate(index[-1],maxim[-1]))
    else index
}





# generateNewName <- function(names.current, new.prefix="gvar")
# {
#     names.current <- as.character(names.current)
#     new.prefix <- as.character(new.prefix)
#     newnames <- c(new.prefix,
#                   paste(gvar, seq(along=names.current), sep=""))
#     newnames[!(newnames %in% names.current)][1]
# }





latticeParseFormula <-
    function(model, data, dimension = 2, subset = TRUE,
             groups = NULL, multiple = FALSE, outer = FALSE,
             subscripts = FALSE, drop = NULL)
    ## this function mostly written by Saikat
{

    ## local function to get length 1 name from expressions (deparse
    ## can split long expressions into several pieces), typically for
    ## subsequent use as labels.  Could be either of the following:

    expr2char <- function(x) paste(deparse(x), collapse = "")
    ## expr2char <- function(x) deparse(x)[1]

    ## by this time, groups is usually already evaluated.  To make
    ## things slightly more convenient, we will now also allow groups
    ## to be of the form groups = ~g, in which case g will be
    ## evaluated now

    if (inherits(groups, "formula"))
    {
        groupVar <- as.character(groups)[2]
        groups <- eval(parse(text = groupVar), data, environment(groups))
    }

    if (is.null(drop)) drop <- TRUE
    if (is.list(drop))
    {
        drop.unused.cond <- if (is.null(drop$cond)) TRUE else drop$cond
        drop.unused.data <- if (is.null(drop$data)) TRUE else drop$data
    }
    else
    {
        drop.unused.cond <- drop
        drop.unused.data <- drop
    }

    parseSide <-
        function(model)
        {
            model.vars <- list()
            while (length(model) == 3 && model[[1]] == as.name("+")) {
                model.vars <- c(model.vars, model[[3]])
                model <- model[[2]]
            }
            rev(c(model.vars, model))
        }

    parseCond <-
        function(model)
        {
            ## WAS: model <- eval(parse(text = paste("~", deparse(model))))[[2]]
            ## but that's not good (PR#7395)
            model <- substitute(~m, list(m = model))[[2]]
            model.vars <- list()
            while (length(model) == 3 && (model[[1]] == as.name("*")
                         || model[[1]] == as.name("+"))) {
                model.vars <- c(model.vars, model[[3]])
                model <- model[[2]]
            }
            rev(c(model.vars, model))
        }

    lrep <-
        function(x, n)
        {
            save.attr <- attributes(x)
            x <- rep(x, n)
            attributes(x) <- save.attr
            x
        }

    concat <-
        function(arglist)
        {
            if (length(arglist) == 1)
                arglist[[1]]
            else if (any(sapply(arglist, is.factor))) {
                factor(unlist(lapply(arglist, as.character)))
            } else if (any(sapply(arglist, is.shingle))) {
                stop("shingles can not be concatenated")
            } else do.call("c", arglist)
        }
    
    if (!inherits(model, "formula"))
        stop("model must be a formula object")
    if (multiple && !outer && !is.null(groups))
    {
        ## ignore multiple = TRUE
        multiple <- FALSE
        warning("multiple = TRUE ignored (groups non-null with outer = FALSE)")
    }

    ans <- if (dimension == 2) {
        list(left = NULL, right = NULL, condition = NULL,
             left.name = character(0), right.name = character(0))
    }
    else if (dimension == 3) {
        list(left = NULL, right.x = NULL, right.y = NULL, condition = NULL,
             left.name = character(0), right.x.name = character(0),
             right.y.name = character(0))
    }
    else stop(paste("invalid dimension : ", dimension))

    
    if (length(model) == 3) {  ## <something> ~ <something>
        if (multiple) {
            varsLHS <- parseSide(model[[2]])
            nLHS <- length(varsLHS)
        } else {
            varsLHS <- list(model[[2]])
            nLHS <- 1
        }
    } else {  ## ~ <something>
        nLHS <- 1
    }
    modelRHS <- model[[length(model)]]
    if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|"))
        modelRHS <- modelRHS[[2]]
        

    ## Note that when dimension = 3, multiple does not apply to RHS

    env <- environment(model)
    modelRHS <- model[[length(model)]]
    if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|")) {
        modelRHS.vars <- parseCond(modelRHS[[3]])
        modelRHS <- modelRHS[[2]]
        if (multiple && dimension == 2) {
            varsRHS <- parseSide(modelRHS)
            nRHS <- length(varsRHS)
        } else {
            varsRHS <- list(modelRHS)
            nRHS <- 1
        }
        ans$condition <- vector("list", length(modelRHS.vars))
        names(ans$condition) <- sapply(modelRHS.vars, expr2char)
        for (i in seq(along = modelRHS.vars)) {
            ans$condition[[i]] <-
                lrep(as.factorOrShingle(eval(modelRHS.vars[[i]], data, env),
                                        subset, drop = drop.unused.cond), nLHS * nRHS)
        }
    } else if (multiple && dimension == 2) {
        varsRHS <- parseSide(modelRHS)
        nRHS <- length(varsRHS)
    } else {
        varsRHS <- list(modelRHS)
        nRHS <- 1
    }

    if (length(model) == 3)
    {

        ## Note: special case if tmp is a matrix. This probably means
        ## we are dealing with the parametric 3-D surface rendering
        ## option in wireframe. subset will be ignored in that case.
        ## allow.multiple must be effectively FALSE in that case

        ans$left.name <- expr2char(model[[2]])
        ans$left <-
            lrep(concat(lapply(varsLHS,
                               function(i) {
                                   tmp <- eval(i, data, env)
                                   if (!is.matrix(tmp))
                                       tmp <-
                                           if (is.factor(tmp) || is.shingle(tmp))
                                               tmp[subset, drop = drop.unused.data]
                                           else
                                               tmp[subset]
                                   if (inherits(tmp, "POSIXt"))
                                       tmp <- as.POSIXct(tmp)
                                   tmp
                               })), nRHS)
    }

    if (dimension == 2)
    {

        ## this part belongs in the first if block, but we need nobs
        ## later in either case, so keeping it here.

        tmp <- eval(varsRHS[[1]], data, env)
        if (is.matrix(tmp)) tmp <- as.data.frame(tmp)
        nobs <- if (is.data.frame(tmp)) nrow(tmp) else length(tmp)

        if (nLHS == 1 && nRHS == 1) {

            if (is.data.frame(tmp))
                ans$right <- tmp[subset, ] ## doesn't do the drop=TRUE thing for factors/shingles
            else
                ans$right <-
                    if (is.factor(tmp) || is.shingle(tmp))
                        tmp[subset, drop = drop.unused.data]
                    else tmp[subset]
        } else {
            ans$right <-
                concat(lapply(varsRHS,
                              function(i) {
                                  tmp <- eval(i, data, env)
                                  tmp <-
                                      if (is.factor(tmp) || is.shingle(tmp))
                                          tmp[subset, drop = drop.unused.data]
                                      else
                                          tmp[subset]
                                  tmp <-
                                      lrep(tmp, nLHS)
                                  if (inherits(tmp, "POSIXt"))
                                      tmp <- as.POSIXct(tmp)
                                  tmp
                              }))
        }
        ans$right.name <- expr2char(modelRHS)
        nRows <- length(ans$right)/(nLHS * nRHS)
    }
    else if (dimension == 3 && length(modelRHS) == 3 &&
             (modelRHS[[1]] == "*" || modelRHS[[1]] == "+"))
    {

        ## Note that when dimension = 3, multiple does not apply to
        ## RHS, but nLHS may be > 1

        ## Note: special case if tmp is a matrix. This probably means
        ## we are dealing with the parametric 3-D surface rendering
        ## option in wireframe. subset will be ignored in that case
        ## allow.multiple must be effectively FALSE in that case


        tmp <- eval(modelRHS[[2]], data, env)
        nobs <- length(tmp)
        if (!is.matrix(tmp)) ## see note above
            tmp <-
                if (is.factor(tmp) || is.shingle(tmp))
                    tmp[subset, drop = drop.unused.data]
                else tmp[subset]
        ans$right.x <- lrep(tmp, nLHS)
        if (inherits(ans$right.x, "POSIXt")) ans$right.x <- as.POSIXct(ans$right.x)
        tmp <- eval(modelRHS[[3]], data, env)
        if (!is.matrix(tmp)) ## see note above
            tmp <-
                if (is.factor(tmp) || is.shingle(tmp))
                    tmp[subset, drop = drop.unused.data]
                else tmp[subset]
        ans$right.y <-
            lrep(tmp, nLHS)
        if (inherits(ans$right.y, "POSIXt")) ans$right.y <- as.POSIXct(ans$right.y)
        ans$right.x.name <- expr2char(modelRHS[[2]])
        ans$right.y.name <- expr2char(modelRHS[[3]])
        nRows <- length(ans$right.x)/nLHS
    }
    else stop("invalid model")
    
    if (nLHS > 1)
        LHSgroups <-
            rep(gl(nLHS, nRows,
                   labels = sapply(varsLHS, expr2char)),
                nRHS)
    if (nRHS > 1)
        RHSgroups <-
            gl(nRHS, nRows*nLHS, labels = sapply(varsRHS, expr2char))
    newFactor <- 
        if (nLHS > 1 && nRHS > 1) {
            factor(paste(LHSgroups, RHSgroups, sep=" * "))
        } else if (nLHS > 1)
            LHSgroups
        else if (nRHS > 1)
            RHSgroups
        else NULL



    if (nLHS == 1 && nRHS == 1) {

        ## subscripts is supposed to provide indices to rows in the
        ## original data frame. When both nLHS and nRHS are 1, this is
        ## simple --- it's seq(length = length of variables)[subset]
        ## (note that groups is never subsetted, so groups[subscripts]
        ## matches subsetted data, indeed, that's how 'groups' is used
        ## in panel functions.

        if (!is.null(groups)) ans$groups <- groups
        if (subscripts) ans$subscr <- seq(length = nobs)[subset]
    }
    else if (outer) {
        if (!is.null(groups)) ans$groups <- rep(groups, nLHS * nRHS)
        if (!is.null(newFactor)) {
            if (is.null(ans$cond))
                ans$condition <- list(newFactor)
            else
                ans$condition[[length(ans$condition) + 1]] <- newFactor
            }
        else stop("newFactor cannot be NULL; you have found a bug!")

        ## note that groups[subscripts] must match rest of (subsetted)
        ## data. This matters when groups is non-null and is repeated
        ## nLHS * nRHS times. It doesn't matter otherwise, but doing
        ## it this way doesn't cause any trouble either. So there.

        if (subscripts)
            ans$subscr <-
                as.vector(matrix(seq(length = nobs * nLHS * nRHS), nrow = nobs)[subset, ])
    }
    else {  ## that is, nLHS * nRHS > 1, outer = FALSE
        if (is.null(groups) && !is.null(newFactor))
            ans$groups <- newFactor
        else stop("newFactor != NULL && groups == NULL does not hold; you have found a bug!")

        ## In this case, groups is something generated artificially,
        ## and is always exactly as long as the rest of the subsetted
        ## variables. So subscr has to be a simple seq, as long as the
        ## new groups.

        if (subscripts) ans$subscr <- seq(length = length(newFactor))

        ## check
        if (length(newFactor) != nRows * nLHS * nRHS)
            stop("Length check mismatch; you have found a bug!")
    }
    ans
}










banking <- function(dx, dy = 1)
{
    if (is.list(dx)) {
        dy <- dx[[2]]
        dx <- dx[[1]]
    }
    if (length(dx)!=length(dy)) stop("Non matching lengths")
    id <- dx!=0 & dy!=0 & !is.na(dx) & !is.na(dy)
    if (any(id)) {
        r  <- abs(dx[id]/dy[id])
        median(r)
    }
    else 1
}










extend.limits <-
    function(lim, length=1, axs = "r",
             prop = if (axs == "i") 0 else lattice.getOption("axis.padding")$numeric)
{
    if (!is.numeric(lim)) NA
    else if(length(lim)==2) {
        if (lim[1]>lim[2]) stop("Improper value of limit")
        if (!missing(length) && !missing(prop))
            stop("length and prop cannot both be specified")
        if (length <= 0) stop("length must be positive")
        if (!missing(length))
        {
            prop <- (as.numeric(length) - as.numeric(diff(lim))) / (2 * as.numeric(diff(lim)))
        }
        if (lim[1]==lim[2]) lim + 0.5 * c(-length,length)
        else {
            d <- diff(as.numeric(lim))
            lim + prop * d * c(-1,1)
        }
    }
    else {
        print(lim)
        stop("improper length of lim")
    }
}







trellis.skeleton <-
    function(formula = NULL,
             cond,
             as.table = default.args$as.table,
             aspect = default.args$aspect,
             between = default.args$between,
             key = NULL,
             legend = NULL,
             page = default.args$page,
             main = default.args$main,
             sub = default.args$sub,
             par.strip.text = default.args$par.strip.text,
             layout = default.args$layout,
             skip = default.args$skip,
             strip = default.args$strip.default,
             strip.left = FALSE,
             xlab = NULL,
             ylab = NULL,
             xlab.default = NULL,
             ylab.default = NULL,
             panel,
             par.settings = NULL,
             index.cond = NULL,
             perm.cond = NULL,
             ...)
{
    default.args <- lattice.getOption("default.args")

    if (is.null(skip)) skip <- FALSE
    foo <-
        list(formula = formula,
             as.table = as.table,
             aspect.fill = aspect == "fill",
             ## key = key,
             legend = construct.legend(legend = legend, key = key),
             panel = panel, 
             page = page,
             layout = layout,
             skip = skip,
             strip = if (is.logical(strip) && strip) "strip.default"
             else strip,
             strip.left = if (is.logical(strip.left) && strip.left) strip.custom(horizontal = FALSE)
             else strip.left,
             xlab = xlab,
             ylab = ylab,
             xlab.default = xlab.default,
             ylab.default = ylab.default,
             main = main,
             sub = sub,
             x.between = 0,
             y.between = 0,
             par.settings = par.settings,
             par.strip.text = par.strip.text,
             index.cond = index.cond,
             perm.cond = perm.cond)

    if (!is.null(between$x)) foo$x.between <- between$x
    if (!is.null(between$y)) foo$y.between <- between$y

    foo$condlevels <- lapply(cond, levels)

    list(foo = foo, dots = list(...))
}








cond.orders <- function(foo, ...) 
    ## function to determine order of panels within a cond. variable
    ## foo: trellis object-to-be

    ## calculate actual values for index.cond and perm.cond.
    ## index.cond can be a function, in which case it would be used to
    ## determing order of levels within conditioning variables

    ## Question: should these be determined at run-time? Wouldn't be
    ## impossible, but has the disadvantage that looking at the
    ## trellis object will be totally uninformative in the default
    ## case (when both would be NULL). In a sense, this is fine, since
    ## having index.cond be a function is similar to having a prepanel
    ## function. After all, the results depend only on the panel
    ## contents, and those cannot be changed via update.

{
    
    ## the following to be used for changing order of conditioning
    ## variables and indexing their levels. The object foo already has
    ## components index.cond and perm.cond as whatever was passed to
    ## the original function call. If these are NULL, suitable
    ## defaults need to be computed. If foo$index.cond is a function,
    ## index.cond has to be computed appropriately.

    index.cond <-
        vector(mode = "list",
               length = length(foo$condlevels))

    for (i in seq(along = foo$condlevels))
        index.cond[[i]] <- seq(along = foo$condlevels[[i]])
    perm.cond <- seq(length = length(foo$condlevels))

    if (!is.null(foo$perm.cond))
    {
        if (all(sort(foo$perm.cond) == perm.cond))
            perm.cond <- foo$perm.cond
        else  stop("Invalid value of perm.cond")
    }
    if (!is.null(foo$index.cond))
    {
        if (is.list(foo$index.cond) && length(foo$index.cond) == length(index.cond))
        {
            for (i in seq(along = foo$condlevels))
                index.cond[[i]] <- index.cond[[i]][foo$index.cond[[i]]]
        }
        else if (is.function(foo$index.cond))
        {
            FUN <- foo$index.cond
            nplots <- length(foo$panel.args)
            panel.order <- numeric(nplots)
            for (count in seq(length = nplots))
            {
                if (is.list(foo$panel.args[[count]]))
                {
                    pargs <- c(foo$panel.args.common, foo$panel.args[[count]], list(...))
                    prenames <- names(formals(FUN))
                    if (!("..." %in% prenames)) pargs <- pargs[prenames]
                    panel.order[count] <- do.call("FUN", pargs)
                }
                else  ## this happens for empty panels
                {
                    panel.order[count] <- NA
                }
            }
            dim(panel.order) <- sapply(foo$condlevels, length)
            for (i in seq(along = foo$condlevels))
                index.cond[[i]] <-
                    order(apply(panel.order, i, mean, na.rm = TRUE))
        }
        else stop("Invalid value of index.cond")
    }
    list(index.cond = index.cond, perm.cond = perm.cond)
}











compute.layout <-
    function(layout, cond.max.level, skip = FALSE)
{
    if (all(skip)) stop("skip cannot be all TRUE")
    number.of.cond <- length(cond.max.level)
    nplots <- prod(cond.max.level)
    
    if (!is.numeric(layout)) {
        layout <- c(0,1,1)
        if (number.of.cond==1) layout[2] <- nplots
        else {
            layout[1] <- cond.max.level[1]
            layout[2] <- cond.max.level[2]
        }
        skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
        plots.per.page <- length(skip) - length(skip[skip])
        layout[3] <- ceiling(nplots/plots.per.page) # + 1
    }
    else if (length(layout)==1)
        stop("layout must have at least 2 elements")
    else if (length(layout)==2)
    {
        if(all(layout<1))
            stop("at least one element of layout must be positive")
        else if (layout[2]==0) stop("inadmissible value of layout")
        
        skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
        plots.per.page <- length(skip) - length(skip[skip])
        layout[3] <- ceiling(nplots/plots.per.page) # + 1 
    }
    else if (length(layout)==3) {
        if(layout[1]<0||layout[2]<1||layout[3]<1)
            stop("invalid value for layout")
    }
    layout
}



