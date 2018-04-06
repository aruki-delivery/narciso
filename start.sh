#!/bin/sh

cd `dirname $0`
. ./sh.config

log inicio

rm $LOCK_FILE 2> /dev/null

RUN_ERL_LOG_ALIVE_MINUTES=60 RUN_ERL_LOG_GENERATIONS=1000 RUN_ERL_LOG_MAXSIZE=10000000 run_erl -daemon /tmp/$INSTANCE . "exec erl -sname $INSTANCE -pa ./ebin ./deps/*/ebin -config ./priv/env -s eb" 

echo "started"
log fim
