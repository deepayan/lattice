

rfs <-
    function(model, layout = c(2,1), xlab = "f-value", ylab = NULL,
             distribution = qunif,
             panel = function(...) {
                 panel.grid(h = -1, v = -1)
                 panel.qqmath(...)
             },
             prepanel = NULL, strip = TRUE, ...)
{

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    fitval <- fitted.values(model) - mean(fitted.values(model))
    resids <- residuals(model)
    
    nf <- length(fitval)
    nr <- length(resids)
    
    data <- list(y = c( fitval, resids),
                 f = c( rep(gettext("Fitted Values minus Mean"), nf),
                 rep(gettext("Residuals"), nr)))

    qqmath(~y|f, data = data, layout = layout, xlab = xlab, ylab = ylab,
           distribution = distribution, panel = panel,
           prepanel = prepanel, strip = strip, ...)
}
