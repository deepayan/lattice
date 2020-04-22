
### Copyright (C) 2001-2020  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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


rfs <-
    function(model, layout = c(2,1), xlab = "f-value", ylab = NULL,
             distribution = qunif,
             panel = function(...) {
                 panel.grid(h = -1, v = -1)
                 panel.qqmath(...)
             },
             prepanel = NULL, strip = TRUE, ...)
{
    ocall <- sys.call(); ocall[[1]] <- quote(rfs)
    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)
    fitval <- fitted.values(model) - mean(fitted.values(model))
    resids <- residuals(model)
    nf <- length(fitval)
    nr <- length(resids)
    data <- list(y = c( fitval, resids),
                 f = c( rep(gettext("Fitted Values minus Mean"), nf),
                 rep(gettext("Residuals"), nr)))
    modifyList(qqmath(~ y | f, data = data, layout = layout, xlab = xlab, ylab = ylab,
                      distribution = distribution, panel = panel,
                      prepanel = prepanel, strip = strip, ...),
               list(call = ocall))
}
