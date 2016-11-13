#!/bin/bash
set -e

elm-make src/Main.elm --output elmtris.js --yes
mkdir -pv out
cp -v elmtris.js index.html style.css out