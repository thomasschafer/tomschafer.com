#!/bin/bash

set -e

echo "Building..."

stack build
stack exec personal-website

echo "Build complete"
