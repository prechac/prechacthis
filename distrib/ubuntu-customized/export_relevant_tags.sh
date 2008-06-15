#!/bin/bash
CHROOT=~/tmp/remaster-root
PT_DIR=$CHROOT/var/www/prechacthis
mkdir -p $CHROOT/var/www/prechacthis
svn export ../../../tags/1.1 $PT_DIR/stable
svn export ../../../tags/multiplex $PT_DIR/multiplex
svn export ../../../trunk/src $PT_DIR/beta
chmod -R a+r $PT_DIR
chmod a+x `find $PT_DIR -type d`
