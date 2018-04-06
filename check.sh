#!/bin/sh

cd `dirname $0`
. ./sh.config

log inicio

if [ -f $LOCK_FILE ]; then
	log servidor em manutencao
else
	if [ `pgrep -c -f "\-sname $INSTANCE"` -eq 0 ]; then
		./start.sh
		log restart feito
	fi
fi

log fim
