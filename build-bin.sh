#!/bin/sh

elm make src/frontend/Main.elm  --output=dist/main.js --optimize;
cp src/frontend/style.css dist;
cp -r src/frontend/icons dist;
stack build --ghc-options='-O2' --copy-bins
