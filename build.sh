#!/bin/sh

mkdir -p dist
cp -r static/* dist
elm make src/Main.elm --output=dist/main.js
