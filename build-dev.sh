#!/bin/sh

hpack &&
cabal exec codegen &&
elm make src/frontend/Main.elm  --output=dist/main.js --debug &&
cp index.html dist &&
cp src/frontend/style.css dist &&
cp -r src/frontend/icons dist &&
cabal build positive:exe:pos -O0 -j
