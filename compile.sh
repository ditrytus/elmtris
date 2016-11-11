#!/bin/bash
set -e

elm-make elmtris.elm --output elmtris.js --yes
cp -v elmtris.js index.html style.css out