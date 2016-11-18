#!/bin/bash
set -e

elm-make Main.elm --output elmtris.js --yes
mkdir -pv out
cp -v elmtris.js index.html style.css coolville-webfont.woff coolville-webfont.woff2 out