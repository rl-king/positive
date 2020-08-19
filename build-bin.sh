#!/bin/sh

elm make src/frontend/Main.elm  --output=dist/main.js --optimize;
stack build --ghc-options='-O2' --copy-bins
