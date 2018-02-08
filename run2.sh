#!/bin/bash

rebar3 release

\cp config/vm.args.tpl config/vm.args

ERL_FLAGS="-config app.config"

export RELX_REPLACE_OS_VARS=true

export NODE_NAME=node2@host2
export COOKIE_NAME=wwwww
export PORT=8888

./_build/default/rel/dora/bin/dora  console