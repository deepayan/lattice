### Copyright 2001-2004  Deepayan Sarkar <deepayan@stat.wisc.edu>
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



summary.trellis <- function(object, ...)
{
    cat("\nCall:\n")
    print(object$call)
    cat("\nY label:\n")
    str(object$ylab)
    cat("\nX label:\n")
    str(object$xlab)

    if (!is.null(names(object$condlevels)))
    {
        cat("\nLevels of Conditioning variables:")
        for (i in seq(along = object$condlevels))
        {
            cat("\n<", i, "> ", names(object$condlevels)[i], "\n", sep = "")
            print(object$condlevels[[i]])
        }
    }
    cat("\n")
    invisible()
}



