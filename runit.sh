#!/bin/bash

EXEPATH=.stack-work/install/x86_64-osx/4b09fd05ca2f71b9cf969b6906a96ebb0ce44fd665d51dbd56cd05f7400f42d9/8.6.5/bin
#EXEPATH=.stack-work/install/x86_64-osx/ea1da75a17412f2c0d18b3add3fe2465d49124cde5a48bb7201c7ff4bcebab93/8.6.5/bin/
${EXEPATH}/bankcheck analyse ~/Dropbox/LPSM/personal/visa/20190701.csv "$@"
#${EXEPATH}/bankcheck analyse ~/Dropbox/LPSM/personal/visa/20190401.csv ~/Dropbox/LPSM/personal/visa/20190701.csv "$@"
#${EXEPATH}/bankcheck analyse ~/Dropbox/LPSM/personal/visa/*.csv "$@"
