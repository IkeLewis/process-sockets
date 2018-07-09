#!/bin/bash -ex

cd $(dirname $(realpath $0))

# For now, generate the documentation from the project README.md

rm -f process-sockets.info
cp ../README.md process-sockets-temp.md
pandoc process-sockets-temp.md -f markdown -t texinfo -s -o process-sockets-temp.texinfo
makeinfo process-sockets-temp.texinfo
emacs -q -batch -l build-docs.el -f build-docs
rm -f process-sockets-temp.*
