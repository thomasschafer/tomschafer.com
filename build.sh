#!/bin/bash

set -e

echo "Building..."

export $(cat .env | grep -v "#" )

stack build
stack exec personal-website

echo "Build complete"
