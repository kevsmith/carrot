#!/bin/sh
cd `dirname $0`
exec erl -sname cqapi_0 -pa $PWD/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin -boot start_sasl -s reloader -s cqapi
