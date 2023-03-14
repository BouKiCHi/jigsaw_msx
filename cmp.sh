#!/bin/sh

mkdir -p jigdata
pasmo jigmain.asm jigdata/jig.com jig.lst
cp test.jig jigdata

