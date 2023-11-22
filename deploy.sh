#!/bin/bash

set -e

./build.sh

echo "Deploying..."

export $(cat .env | xargs)

netlify deploy --site $SITE_NAME --dir ./out --prod

echo "Deployment complete"
