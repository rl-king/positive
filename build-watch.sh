#!/bin/sh

find src/frontend/ -name "*.elm" | entr elm make --debug src/frontend/Main.elm --output=dist/main.js & \
find src/frontend/ -name "*.svg" | entr cp -r src/frontend/icons dist & \
find src/frontend/ -name "*.css" | entr cp src/frontend/style.css dist & \
livereload dist -e 'css, js' -w 500
