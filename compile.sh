#!/bin/bash
set -e

mkdir -pv out
elm-make src/elm/Main.elm --output out/elmtris.js --yes
cp -v index.html style.css coolville-webfont.woff coolville-webfont.woff2 out