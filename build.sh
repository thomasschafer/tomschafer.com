#!/bin/bash

set -e

echo "Building..."

export $(cat .env | grep -v "#" )

stack build
stack exec personal-website

sass --no-source-map --style=compressed ./src/styles/styles.scss ./out/styles.css

echo "Build complete"
