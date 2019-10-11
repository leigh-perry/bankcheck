#!/bin/bash

EXEPATH=$(find . -iname bankcheck | grep install | grep bin)

#${EXEPATH} totals -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/20*.csv "$@"
${EXEPATH} since 20190901 -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/20*.csv "$@"
