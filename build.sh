#!/bin/bash

set -e

echo "Building..."

nix-shell --pure --run "make full-build"

echo "Build complete"
