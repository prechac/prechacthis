#!/bin/bash
CHROOT=$1
CHROOTwww=$CHROOT/var/www
PT_DIR=$CHROOTwww/prechacthis
REPO="https://prechacthis.svn.sourceforge.net/svnroot/prechacthis"

mkdir -p $CHROOT/var/www/prechacthis
svn export $REPO/tags/1.1 $PT_DIR/stable
svn export $REPO/tags/multiplex $PT_DIR/multiplex
svn export $REPO/trunk/src $PT_DIR/beta
chmod -R a+r $PT_DIR
chmod a+x `find $PT_DIR -type d`
