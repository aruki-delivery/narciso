#!/bin/sh

cd `dirname $0`
. ./sh.config

log inicio

erl_call -sname $INSTANCE -a "init stop" > /dev/null 2> /dev/null

touch $LOCK_FILE

echo "stopped"
log fim
