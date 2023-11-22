#!/bin/bash

set -e

./build.sh

echo "Deploying..."

netlify deploy --site $ NETLIFY_SITE_NAME --dir ./out --prod

echo "Deployment complete"
