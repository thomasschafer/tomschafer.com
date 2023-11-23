#!/bin/bash

set -e

./build.sh

export $(cat .env | grep -v "#" )

echo "Deploying..."

netlify deploy --site $NETLIFY_SITE_NAME --dir ./out --prod

echo "Deployment complete"
