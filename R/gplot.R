

### EXPERIMENTAL: a general 'grouped display' function gplot that
### could potentially be used to display any R object using S3 method
### dispatch


## The idea of the gplot function is as follows: It can be thought of
## as a new plot method, typically to be applied to data frames.  It
## would eventually call a lattice function, but there would be a few
## extra arguments that would be pre-processed.  These are modeled on
## the groupedData() constructor in nlme.

## to keep things conceptually as simple as possible, here's what
## would happen with a call of the form gplot(x, ...):

## 1 start with list: callList <- list(data = x)
## 2 update callList with gplotArgs(x, ...)
## 3 callList MUST now have a component called plotFun.  Do a
##   do.call(plotFun, <rest of callList>)

## In practice, the only non-trivial stuff will be done by
## gplotArgs().  For now, we will only have a gplotArgs method for
## data frames.  For objects without specific methods, the default
## will be to use the "gplot.args" attribute



gplotArgs <- function(x, ...)
    UseMethod("gplotArgs")


gplotArgs.default <- 
    function(x, ...)
{
    updateList(attr(x, "gplot.args"), list(...))
}

gplot <- function(x, ...)
{
    callList <- updateList(list(data = x), gplotArgs(x, ...))
    plotFun <- callList$plotFun
    if (!is.null(callList$display.formula))
    {
        callList$formula <- callList$display.formula
        callList$display.formula <- NULL
    }
    if (is.null(plotFun))
        stop("display function not specified")
    else
    {
        callList$plotFun <- NULL
        do.call(plotFun, callList)
    }
}




### Now, the question is, (1) whether to have the method for data
### frames (which is what's of immediate interest) here or in lme4.
### It doesn't really make sense to have it in lme4, since we won't be
### defining a new class.  But most of the features would be borrowed
### from nlme (2) whether to have methods for any other class (like
### 'lm' maybe)



### data frame method: gplotArgs.data.frame(x, <special args>, ...)

## 4 levels of information:
##    o attr(x, "ginfo")
##    o <special args>
##    o attr(x, "gplot.args")
##    o ...
## the first 2 determine a reasonable list, overridden by the 3rd and
## then the 4th



## should we have a non-generic "gplotArgs<-" for setting the
## "gplot.args" attribute?



## First, we need some good default panel functions.  These need to
## depend on

## (1) the type of display formula -
##      o factor ~ numeric   [ model: numeric ~ 1       | factor ]
##      o numeric ~ numeric  [ model: numeric ~ numeric | factor ]
##      o  ~ numeric         [ model:  factor ~ numeric | factor ] NEW


panel.df.fn <-               # factor ~ numeric
    function(x, y, groups = NULL, ...)
{
    panel.dotplot(x=x, y=y, groups = groups, ...)
}

panel.df.nn <-               # numeric ~ numeric
    function(x, y, lines = TRUE, ...)
{
    panel.xyplot(x, y, ...)
    if (lines)
    {
        y.avg <- tapply(y, x, mean) # lines through average y
        y.avg <- y.avg[!is.na(y.avg)]
        if (length(y.avg) > 0)
        {
            xvals <- as.numeric(names(y.avg))
            ord <- order(xvals)
            panel.xyplot(xvals[ord], y.avg[ord], type = "l", ...)
        }
    }
}



panel.df <- # combines above 2, should be called
    function(x, y = NULL, groups = NULL, grid = TRUE, ...)
{
    yNull <- is.null(y)
    groupsNull <- is.null(groups)
    xFactor <- is.factor(x)
    yFactor <- is.factor(y)

    if (yNull) ## for factor response, may not be in use yet
    {
        if (groupsNull) panel.densityplot(x = x, ...)
        else panel.superpose(x = x, groups = groups,
                             panel.groups = "panel.densityplot", ...)
    }
    else if (!xFactor && !yFactor) ## numeric ~ numeric
    {
        if (grid) panel.grid()
        if (groupsNull) panel.df.nn(x = x, y = y, ...)
        else panel.superpose(x = x, groups = groups,
                             panel.groups = panel.df.nn, ...)
    }
    else if (yFactor && !xFactor)
    {
        panel.df.fn(x = x, y = y, groups = groups, ...)
    }
    else stop("can't handle both x and y being factors yet")
    invisible()
}


## display formula

## generally, the display is determined by the arguments 'formula' and
## 'groups'.  In the nlme scheme, the formula associated with the data
## is a model formula, not a display formula.  The display is further
## determined by the 'outer' and 'inner' arguments.  

## Let's try to outline a strategy for this.

## attr(x, "ginfo") can contain things used in nlme's groupedData.

## formula can be of the form 'y ~ x | id' (only one variable in 'id',
## although it can be an interaction). inner=~a becomes default
## grouping factor. outer=~b+c used for ordering levels of id by
## values of y.  outer can be also an argument to
## gplotArgs.data.frame.  If TRUE, it's equivalent to outer =
## ginfo$outer, or it could be =~e+f.  In either case, it becomes the
## conditioning variables, id becomes groups (inner would then be
## ignored)

## In the formula itself, if x is a factor, the display formula
## becomes id ~ y, with groups = x (unless x = 1, then no groups)


## utility functions (defined in nlme, but need to be redefined here)
## (check)

getResponseFormula <- function(formula)
{
    if (length(formula) == 3) as.character(formula[2])
    else stop("invalid formula")
}

getCovariateFormula <- function(formula)
{
    RHS <- 
        if (length(formula) == 3) as.character(formula[3])
        else if (length(formula) == 2) as.character(formula[2])
        else stop("invalid formula")
    RHS <- strsplit(RHS, " | ", fixed = TRUE)[[1]]
    RHS[1]
}

getGroupsFormula <- function(formula)
{
    RHS <- 
        if (length(formula) == 3) as.character(formula[3])
        else if (length(formula) == 2) as.character(formula[2])
        else stop("invalid formula")
    RHS <- strsplit(RHS, " | ", fixed = TRUE)[[1]]
    RHS[2]
}

## FIXME: how to make sure whitespace is removed? Seems to be taken
## care of by as.character.formula


getTypeInDF <- function(x, data)
{
    if (x == "1") "one"
    else if (is.factor(data[[x]])) "factor"
    else if (is.numeric(data[[x]])) "numeric"
    else stop(paste("don't recognize", class(data[[x]])))
}

## should work with expressions like log(height) as well
getTypeInDF2 <- function(x, data)
{
    x <- eval(parse(text = x), data, parent.frame())
    if (length(x) == 1 && x == 1) "one"
    else if (is.factor(x)) "factor"
    else if (is.numeric(x)) "numeric"
    else stop(paste("don't recognize", class(x)))
}




## S3 method

## basic idea: get defaults from "ginfo" attribute, then overwrite by
## "gplot.args" attribute, followed by ...

gplotArgs.data.frame <-
    function(x, display.formula, outer = FALSE,
             groups = NULL,
             ...,
             subset = TRUE)
{

    ## The final result should contain only standard trellis args,
    ## with a special component plotFun, and optionally a
    ## display.formula, which overrides formula.  However, for data
    ## frames, there are some other issues.

    ## The "ginfo" attribute can only contain info traditionally in
    ## nlme groupedData objects.  This, along with explicit arguments
    ## here, will be used to create a list of trellis args.  These can
    ## be overridden by the "gplot.args" attribute, and then by
    ## ... here.


    ginfo <- attr(x, "ginfo")
    gplot.args <- attr(x, "gplot.args")
    ## equivalent to default method if ginfo is NULL
    if (is.null(ginfo) || !is.list(ginfo))
        return(updateList(gplot.args, list(...)))

    ## First (longish) task: determine display formula and groups

    ## groups is by far the most irritating thing to handle.  The only
    ## options I can think of: (1) evaluate groups here and pass it on
    ## and (2) change lattice to allow groups to be a formula.  Use
    ## (1) for now

    groups <- eval(substitute(groups), x, parent.frame()) # typically NULL

    ## subset poses a similar problem

    subset <- eval(substitute(subset), x, parent.frame())


    if (missing(display.formula))
        display.formula <- gplot.args$display.formula

    ## major step: get the display formula, but only if it's NULL
    model.formula <- ginfo$formula
    if (!is.null(display.formula))
        ## no point in jumping through hoops
    {
        ans <- list(display.formula = display.formula)
    }
    else if (!is.null(model.formula)) ## the interesting stuff
    {
        vars <-
            list(resp = getResponseFormula(model.formula),
                 cov = getCovariateFormula(model.formula),
                 grp = getGroupsFormula(model.formula))
        varTypes <-
            lapply(vars, getTypeInDF2, data = x)

        ## Next step depends on varTypes

        ## case 1: cov = "one" - grp ~ resp
        ## case 2: cov = "factor" - grp ~ resp, groups = cov
        ## case 1: cov = "numeric" - resp ~ cov | grp

        fc <- switch(varTypes$cov,
                     one = paste(vars$grp, "~", vars$resp),
                     factor = paste(vars$grp, "~", vars$resp),
                     numeric =
                     paste(vars$resp, "~", vars$cov, "|", vars$grp))
        ans <-
            list(display.formula = as.formula(fc))

        if (varTypes$cov == "factor" && is.null(groups))
            groups <-
                eval(parse(text = vars$cov), x, parent.frame())
    }
    else 
    {
        stop("no formula!")
    }

    ## determine default plot function based on display.formula

    dvars <-
        list(resp = getResponseFormula(ans$display.formula),
             cov = getCovariateFormula(ans$display.formula))
    dvarTypes <-
        lapply(dvars, getTypeInDF2, data = x)
    if (dvarTypes$resp == "numeric" && dvarTypes$cov == "numeric")
        plotFun <- "xyplot"
    else if (dvarTypes$resp == "factor" && dvarTypes$cov == "numeric")
        plotFun <- "dotplot"
    else {
        str(dvarTypes)
        stop("unsupported combination")
    }

    ## other stuff in ginfo?

    ans <-
        updateList(ans,
                   list(plotFun = plotFun,
                        panel = panel.df,
                        groups = groups,
                        subset = subset))
                      
    ans <- updateList(ans, attr(x, "gplot.args")) ## leave this out?
    updateList(ans, list(...))
}





## new version of groupedData

groupedData <-
    function(formula, data,
             order.groups = TRUE,
             FUN = function(x, y, ...) max(y, na.rm = TRUE),
             outer = NULL, inner = NULL,
             labels = list(), units = list(), ...)

### outer: only used to order panels - same level of outer clustered
### inner: equivalent of groups. Not sure what more than one means
### ...: stored separately, can be used to override gplotArgs calculations
{
    attr(data, "ginfo") <-
        list(formula = formula,
             order.groups = order.groups,
             FUN = FUN,
             outer = outer,
             inner = inner,
             labels = labels,
             units = units)
    attr(data, "gplot.args") <- list(...)
    data
}


