#!/bin/sh

elm make src/frontend/Main.elm  --output=dist/main.js --optimize;
cp src/frontend/style.css dist;
cp -r src/frontend/icons dist;
stack build --ghc-options='-O2 -j +RTS -A128m -n2m -RTS' --copy-bins
