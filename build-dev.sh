#!/bin/sh

find src/backend/ -name "*.hs" | entr -r stack run -- --dev & \
find src/frontend/ -name "*.elm" | entr elm make --debug src/frontend/Main.elm --output=dist/main.js & \
find src/frontend/ -name "*.css" | entr cp src/frontend/style.css dist & \
livereload dist -e 'css, js' -w 500
