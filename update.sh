#!/usr/bin/env sh

## Usage: ./update.sh 
## 
## has to be run in current directory
## 
## Tasks: 
##   (1) update translation templates 
##   (2) run msgmerge on existing translation catalogs
##   (2) update SvnLog [actually create a file with latest entries]


R_PROG=R
PKG=lattice

export LC_ALL=C
echo "library(tools); xgettext2pot('.', 'po/R-${PKG}.pot')" | ${R_PROG} --vanilla --silent

LANGUAGES="en@quot fr de ko pl_PL"

for LANG in ${LANGUAGES}; do 
    echo "Updating translations for ${LANG}"
    POFILE=po/R-${LANG}.po 
    msgmerge --no-wrap --update ${POFILE} po/R-${PKG}.pot
    PODIR=inst/po/${LANG}/LC_MESSAGES
    MOFILE=${PODIR}/R-${PKG}.mo
    mkdir -p ${PODIR}
    msgfmt --statistics -c ${POFILE} -o ${MOFILE}
done



## Note: -o is relatively new in grep
LASTLOG=`grep -o -m 1 "r[0-9]* |" SvnLog | sed -e 's/[^0-9]//g'`
svn log -v -r HEAD:${LASTLOG} > SvnLog.update

