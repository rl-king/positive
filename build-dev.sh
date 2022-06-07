#!/bin/sh

hpack &&
cabal exec pos-codegen &&
elm make src/frontend/Main.elm  --output=dist/main.js --debug &&
cp index.html dist &&
cp src/frontend/style.css dist &&
cp -r src/frontend/icons dist &&
cabal install positive:exe:pos -O0 -j --overwrite-policy=always --ghc-options="-j +RTS -A64m -n2m -RTS"
