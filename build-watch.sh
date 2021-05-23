#!/bin/sh

find src/frontend/ -name "*.elm" | entr -c elm make src/frontend/Main.elm --output=dist/main.js & \
find src/frontend/ -name "*.svg" | entr -c cp -r src/frontend/icons dist & \
find src/frontend/ -name "*.css" | entr -c cp src/frontend/style.css dist & \
livereload dist -e 'css, js' -w 500
