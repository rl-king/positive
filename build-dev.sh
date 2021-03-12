#!/bin/sh

stack build --fast --ghc-options "-j4 +RTS -A128m -n2m -RTS -Wall" && \
stack runghc -- -isrc src/codegen/CodeGen.hs && \
elm make --debug src/frontend/Main.elm --output=dist/main.js && \
cp -r src/frontend/icons dist && \
cp src/frontend/style.css dist
