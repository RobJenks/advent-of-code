SCRIPT=`which $0`
WDIR=`dirname $SCRIPT`

$WDIR/build.sh && $WDIR/../main
