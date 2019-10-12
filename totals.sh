#!/bin/bash

EXEPATH=$(find . -iname bankcheck | grep install | grep bin)
# set -x
${EXEPATH} totals -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/20*.csv "$@"
