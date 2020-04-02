#!/usr/bin/env sh

## Usage: ./update.sh 
## 
## Must be run in current directory
## 
## Tasks: 

## (1) update translations

export LC_ALL=en_US.UTF-8
Rscript --no-init-file -e "tools::update_pkg_po('.')"

## (2) extract git logs

git log --name-status > ChangeLog.update

