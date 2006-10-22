#!/usr/bin/env sh

## Usage: ./update.sh 
## 
## has to be run in current directory
## 
## Tasks: 
##   (1) update translation templates 
##   (2) run msgmerge on existing translation catalogs
##   (2) update SvnLog [actually create a file with latest entries]

R_PROG=R-2.4
export LC_ALL=C
echo "library(tools); xgettext2pot('.', 'po/R-lattice.pot')" | ${R_PROG} --vanilla --silent
for f in po/R-*.po; do msgmerge --no-wrap --update $f po/R-lattice.pot; done

## Note: -o is relatively new in grep
LASTLOG=`grep -o -m 1 "r[0-9]* |" SvnLog | sed -e 's/[^0-9]//g'`
svn log -v -r HEAD:${LASTLOG} > SvnLog.update

