#!/bin/bash -x
USER=$(whoami)

CHROOT=/home/$USER/tmp/remaster-root

echo "chmod -R a+rw $CHROOT/etc/skel $CHROOT/var"
sudo chmod -R a+rw $CHROOT/etc/skel $CHROOT/var/www

sudo ./export_relevant_tags.sh $CHROOT

sudo echo startx >> $CHROOT/etc/skel/.bashrc

AUTOSTART_DIR=$CHROOT/etc/skel/.config/autostart
sudo mkdir -p $AUTOSTART_DIR
sudo cp prechacthis.desktop $AUTOSTART_DIR

sudo chmod -R a+rw $CHROOT/etc/skel $CHROOT/var/www
