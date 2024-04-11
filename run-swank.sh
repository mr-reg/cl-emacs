#!/bin/sh
# Copyright (C) 2024 by Gleb Borodulia
# Author: Gleb Borodulia <mr.reg@mail.ru>
set -e
CCL_PATH=./.ccl/
rm -Rf $CCL_PATH || echo $CCL_PATH directory does not exist
mkdir -p $CCL_PATH
cp -R /ccl/* $CCL_PATH
if [ "$ASDF_ONLY" = "1" ]
then
    echo "starting asdf only"
    $CCL_PATH/lx86cl64 --no-init --eval "(defvar *asdf-only* t)" --load run-swank.lisp
else
    $CCL_PATH/lx86cl64 --no-init --load run-swank.lisp
fi
# ecl --norc --load run-swank.lisp
# sbcl --no-sysinit --no-userinit --load run-swank.lisp
