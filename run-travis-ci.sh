#!/bin/sh

# This script was shamelessly lifted from the dash project.
#
# See https://github.com/magnars/dash.el

cd "$(dirname "$0")"

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

set_default EMACS "$(which emacs)"

echo "*** Emacs version ***"
echo "EMACS =" $(which $EMACS)
$EMACS --version
echo

exec ./run-tests.sh
