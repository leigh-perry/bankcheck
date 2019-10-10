#!/bin/bash

EXEPATH=$(find . -iname bankcheck | grep install | grep bin)

#${EXEPATH} totals -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/20190701.csv "$@"
${EXEPATH} totals -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/20190401.csv ~/Dropbox/LPSM/personal/visa/20190701.csv "$@"
#${EXEPATH} totals -w ~/Dropbox/LPSM/personal/visa/whitelist.csv ~/Dropbox/LPSM/personal/visa/*.csv "$@"
