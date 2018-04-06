#!/bin/sh

cd `dirname $0`
. ./sh.config

log inicio

FULL_DIR=`pwd`

case "$1" in
	uninstall)
		log desinstalacao
		crontab -l | sed -e "/Server $INSTANCE/d;\#$FULL_DIR/check.s#d" | crontab -
		;;
	install)
		log instalacao
		(crontab -l ; echo "# Server $INSTANCE running check every minute" ; echo "* * * * * $FULL_DIR/check.sh > /dev/null 2> /dev/null") | crontab -
		;;
	*)
		echo "Usage: `basename $0` install|uninstall"
		;;
esac

log fim
