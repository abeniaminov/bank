#!/bin/sh

NAME="bank" # main app and node name
COOKIE=QKDFVRMXUFKAFWQFMAJA1
CONFIG=bank_1


# get our IP address
IP=`ifconfig  | grep 'inet addr:' | grep -v '127.0.0.1'  --max-count=1 | cut -d: -f2 | awk '{print $1}'`

if [ x"" = x"${IP}" ]; then
    IP=`ifconfig  | grep 'inet ' | grep -v '127.0.0.1'  --max-count=1 | cut -d\  -f2`
fi;

if [ x"" = x"${IP}" ]; then
    IP="localhost"
fi;

MAIN_APP=$NAME"_app"
MAIN_NODE="${NAME}_1@$IP"
CTRL_NODE="$NAME`date +_nodeclt_%H_%M_%S_%N`b@$IP"
SERVER_ROOT=`pwd`
CMD="rpc:call\($CTRL_NODE,conf_server,reload,[]\)."
# erl arguments doc: http://www.erlang.org/doc/man/erl.html
# +K true        enable kernel poll, deal with high number of open file descriptors
# +A 128         number of threads in async thread pool
# +P 134217727   maximum number of simultaneously existing processes
ERL_ARGS="+K true +A 128 +P 2000000"

export ERL_MAX_ETS_TABLES=140000
export ERL_LIBS=$SERVER_ROOT/_build/default/lib

ACTION=$1

if [ "" = "$ACTION"   ]; then
    echo "
        USAGE:
            $0  <command> [<arg> ...]
            MANAGE
                start [-detached]  - start up node
                startd             - alias to 'start -detached'
                stop               - stops application and halts the node
                version            - request application version
        "
else
    echo "
        INFO:
            MAIN_APP:            $MAIN_APP
            MAIN_NODE:           $MAIN_NODE
            CTRL_NODE:           $CTRL_NODE
            CONFIG:              $CONFIG
            ERL_ARGS:            $ERL_ARGS
            ERL_MAX_ETS_TABLES:  $ERL_MAX_ETS_TABLES
            ERL_LIBS:            $ERL_LIBS
            SERVER_ROOT:         $SERVER_ROOT
            IP:                  $IP
        "
    cd $SERVER_ROOT
    case "$ACTION" in
        "start")
            shift
            erl \
            +pc unicode \
            -pa $SERVER_ROOT/_build/default/lib/bank/ebin \
            -pa $SERVER_ROOT/_build/default/lib/sync/ebin \
            -boot start_sasl \
            -config ${CONFIG} \
            -name ${MAIN_NODE} \
            -setcookie ${COOKIE} \
            -s ${MAIN_APP} \
            ${ERL_ARGS} \
            "$@"
            ;;
        "startd")
            shift
            erl \
            +pc unicode \
            -pa $SERVER_ROOT/-build/default/lib/bank/ebin \
            -boot start_sasl \
            -config ${CONFIG} \
            -name ${MAIN_NODE} \
            -setcookie ${COOKIE} \
            -s ${MAIN_APP} \
            ${ERL_ARGS} \
            -detached \
            "$@"
            ;;
        "stop") escript \
            stop.sh  ${CTRL_NODE} ${MAIN_NODE} ${COOKIE} \
            "$@"
            ;;
    esac
fi;
