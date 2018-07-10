#!/bin/bash -ex

cd $(dirname $(realpath $0))

# For now, generate the documentation from the project README.md

rm -f process-sockets.info
cp ../README.md process-sockets.md
pandoc process-sockets.md -f markdown -t texinfo -s -o process-sockets.texinfo
makeinfo process-sockets.texinfo
emacs -q -batch -l build-docs.el -f build-docs
rm -f process-sockets.md process-sockets.texinfo
