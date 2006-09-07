
### Copyright (C) 2000-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>,
###
### This file is part of the lattice package for R.  It is made
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
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA



.LatticeEnv <- new.env()
assign("lattice.status",  list(), env = .LatticeEnv)
assign("lattice.theme",   list(), env = .LatticeEnv)
assign("lattice.options", list(), env = .LatticeEnv)
assign("last.object",     NULL,   env = .LatticeEnv)


.onLoad <- function(libname, pkgname) 
{
    ## library.dynam("lattice", pkgname, libname )
    lattice.options(.defaultLatticeOptions())
    lattice.setStatus(.defaultLatticeStatus())
}

.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lattice", libpath)




## .First.lib will be used if the NAMESPACE file is missing.  This is
## useful during development, thanks to C-c C-l in Emacs/ESS. It won't
## be used if a NAMESPACE file is present.  Note: Due to registration
## of C functions done in the NAMESPACE file, wireframe (and possibly
## cloud) won't work in this scenario.


.First.lib <- function(lib, pkg) 
{
    cat(gettext("Note: you shouldn't be seeing this message unless\nyou are using a non-standard version of lattice"),
        fill = TRUE)
    library.dynam(pkg, pkg, lib )
    if (!require("grid")) stop("The grid package couldn't be loaded.\nPlease check your installation of R")
    lattice.options(.defaultLatticeOptions())
    lattice.setStatus(.defaultLatticeStatus())
}




