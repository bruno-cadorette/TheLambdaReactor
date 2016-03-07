#!/bin/bash
elm-package install --yes
cp src/Frontend/external/Collage.elm elm-stuff/packages/elm-lang/core/3.0.0/src/Graphics
cp src/Frontend/external/Collage.js elm-stuff/packages/elm-lang/core/3.0.0/src/Native/Graphics
elm-make
