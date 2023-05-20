#!/usr/bin/env pwsh
# -*- coding: utf-8 -*-

# Set Variables
$GH_USER="jimbrig"
$GH_REPO="shinymde"
$DOCKER_TAG="latest"
$DOCKER_DESCRIPTION="ShinyMDE Container Image"
$GH_PAT=$Env:GITHUB_API_TOKEN

$IMAGE_SOURCE="https://github.com/" + $GH_USER + "/" + $GH_REPO
$IMAGE_REF="ghcr.io/$GH_USER/$GH_REPO" + ':' + $DOCKER_TAG

# Create Container
RScript -e "golem::add_dockerfile()"

# Login
echo $GH_PAT | docker login ghcr.io -u $GH_USER --password-stdin

# Build
docker build `
 --label "org.opencontainers.image.source=https://github.com/$GH_USER/$GH_REPO" `
 --label "org.opencontainers.image.description=$DOCKER_DESCRIPTION" `
 --label "org.opencontainers.image.licenses=MIT" `
 -t $IMAGE_REF `
 .

# Push
docker push $IMAGE_REF
