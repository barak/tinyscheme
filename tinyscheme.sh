#! /bin/sh
SCHEME=`dirname $0`/scheme
RLWRAP=`which rlwrap 2>/dev/null`
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:."
test -x "$SCHEME" &&
exec $RLWRAP ./scheme "$@"
echo >&2 'TinyScheme not found!'
exit 1
