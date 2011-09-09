#!/bin/sh

find apps/sip/ -iname "*.[eh]rl" | xargs -n1 sed -i -e "s/ \{1,\}$//"
