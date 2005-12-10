#!/usr/bin/env sh


## Usage: ./update.sh 
##

## Tasks: (1) update po files (2) update SvnLog

R_PROG=R-2.2
echo "library(tools); xgettext2pot('.', 'po/R-lattice.pot')" | ${R_PROG} --vanilla --silent

LASTLOG=`grep -o -m 1 "r[0-9]* |" SvnLog | sed -e 's/[^0-9]//g'`

svn log -v -r HEAD:${LASTLOG} > SvnLog.update







