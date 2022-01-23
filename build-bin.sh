#!/bin/sh

hpack &&
cabal exec codegen &&
elm make src/frontend/Main.elm  --output=dist/main.js --optimize &&
cp index.html dist &&
cp src/frontend/style.css dist &&
cp -r src/frontend/icons dist &&
cabal install positive:exe:pos -O2 -j --overwrite-policy=always
