#!/bin/sh
DOCDIR=`dirname $0`
texi2pdf --clean $DOCDIR/blockfort-internals.texinfo
texi2pdf --clean $DOCDIR/cl-blockfort.texinfo
