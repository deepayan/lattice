




modifyList <-
    function(x, val)
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    for (v in names(val))
    {
        if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
            x[[v]] <- modifyList(x[[v]], val[[v]])
        else 
            x[[v]] <- val[[v]]
    }
    x
}

