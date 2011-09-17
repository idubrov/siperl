#!/bin/sh

find apps/*/src apps/*/test demos/*/src  -iname "*.[eh]rl" | xargs -n1 sed -i -e "s/ \{1,\}$//"
