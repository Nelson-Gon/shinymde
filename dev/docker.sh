#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# Set Variables
GH_USER="jimbrig"
GH_REPO="shinymde"
DOCKER_TAG="latest"
DOCKER_DESCRIPTION="ShinyMDE Container Image"
GH_PAT=$GITHUB_API_TOKEN

# Create Container
RScript -e "golem::add_dockerfile()"

# Login
echo $GH_PAT | docker login ghcr.io -u $GH_USER --password-stdin

# Build
docker build \
 --label "org.opencontainers.image.source=https://github.com/$GH_USER/$GH_REPO" \
 --label "org.opencontainers.image.description=$DOCKER_DESCRIPTION" \
 --label "org.opencontainers.image.licenses=MIT" \
 -t ghcr.io/$GH_USER/$GH_REPO:$DOCKER_TAG \
 .

# Push
docker push ghcr.io/$GH_USER/$GH_REPO:$DOCKER_TAG
