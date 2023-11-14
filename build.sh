#!/bin/bash

echo "Building..."

stack build
stack exec personal-website

echo "Build complete"
