#!/bin/sh
# Copyright (C) 2024 by Gleb Borodulia
# Author: Gleb Borodulia <mr.reg@mail.ru>
set -e
CCL_PATH=./.ccl/
rm -Rf $CCL_PATH || echo $CCL_PATH directory does not exist
mkdir -p $CCL_PATH
cp -R /ccl/* $CCL_PATH
rm -Rf ./systems systems.csv
if [ ! -z "$OCICL_REGISTRY" ]
then
    echo using custom ocicl registry $OCICL_REGISTRY
    echo "$OCICL_REGISTRY" > ~/.local/share/ocicl/ocicl-registry.cfg 
fi
if [ ! -z "$OCICL_GLOBALDIR" ]
then
    echo using ocicl $OCICL_GLOBALDIR
    mkdir -p $OCICL_GLOBALDIR
    ocicl setup $OCICL_GLOBALDIR
fi

if [ "$ASDF_ONLY" = "1" ]
then
    echo "starting asdf only"
    $CCL_PATH/lx86cl64 --eval "(defvar *asdf-only* t)" --load run-swank.lisp
else
    $CCL_PATH/lx86cl64 --load run-swank.lisp
fi
# ecl --norc --load run-swank.lisp
# sbcl --no-sysinit --no-userinit --load run-swank.lisp
