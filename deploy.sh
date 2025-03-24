#!/bin/bash

set -e

echo "Deploying..."

nix-shell --pure --run "make deploy"

echo "Deployment complete"
