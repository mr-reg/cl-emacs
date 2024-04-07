#!/bin/sh
# Copyright (C) 2024 by Gleb Borodulia
# Author: Gleb Borodulia <mr.reg@mail.ru>
if [ "$ASDF_ONLY" = "1" ]
then
    echo "starting asdf only"
    lx86cl64 --no-init --eval "(defvar *asdf-only* t)" --load run-swank.lisp
else
    lx86cl64 --no-init --load run-swank.lisp
fi
# ecl --norc --load run-swank.lisp
# sbcl --no-sysinit --no-userinit --load run-swank.lisp
