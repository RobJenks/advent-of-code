SCRIPT=`which $0`
WDIR=`dirname $SCRIPT`

ghc -dynamic -o $WDIR/../main $WDIR/../main.hs
