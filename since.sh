#!/bin/bash

EXEPATH=$(find . -iname bankcheck | grep install | grep bin)
# set -x
${EXEPATH} since $1 -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/20*.csv 
