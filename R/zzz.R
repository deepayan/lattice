
### Copyright 2000-2003 Deepayan Sarkar <deepayan@stat.wisc.edu>,
###
### This file is part of the lattice library for R.  It is made
### available under the terms of the GNU General Public License,
### version 2, or at your option, any later version, incorporated
### herein by reference.
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



.onLoad <- function(lib, pkg) {
    library.dynam(pkg, pkg, lib )
    ## Note: grid functions will not be visible
    ## if (!require(grid))
    ##    stop("lattice requires grid, but grid couldn't be loaded")


    ## this has to be done after .LatticeEnv has been defined (!)
    lattice.options(.defaultLatticeOptions())
    lattice.setStatus(.defaultLatticeStatus())
}

.LatticeEnv <- new.env()

## Need global variable to handle more in print.trellis

##assign(".lattice.print.more", FALSE, env = .LatticeEnv) - delegated to status list

assign("lattice.status", list(), env = .LatticeEnv)
assign("lattice.theme", list(), env = .LatticeEnv)
assign("lattice.options", list(), env = .LatticeEnv)
assign("last.object", NULL, env = .LatticeEnv)


.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lattice", libpath)









## old (pre NAMESPACE version). If you need to rename the NAMESPACE
## file (for testing, say), also uncomment the code below. Nothing
## else need be changed.




#.First.lib <- function(lib, pkg) {
#    library.dynam(pkg, pkg, lib )
#    if (!require(grid))
#        stop("lattice requires grid, but grid couldn't be loaded")
#    ## this has to be done after .LatticeEnv has been defined (!)
#    lattice.options(.defaultLatticeOptions())
#    lattice.setStatus(.defaultLatticeStatus())
#}




