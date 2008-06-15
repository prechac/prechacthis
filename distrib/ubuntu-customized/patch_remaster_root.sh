#!/bin/bash
export CHROOT=~/tmp/remaster-root
./export_relevant_tags.sh
echo startx >> $CHROOT/etc/skel/.bashrc
