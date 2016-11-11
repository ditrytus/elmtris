#!/bin/bash
set -e

elm-make -yes elmtris.elm --output elmtris.js
cp -v elmtris.js index.html style.css out