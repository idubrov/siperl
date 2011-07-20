#!/bin/sh

find lib/sip/ -iname "*.[eh]rl" | xargs -n1 sed -i -e "s/ \{1,\}$//"
