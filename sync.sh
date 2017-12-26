#!/bin/bash
set -eux -o pipefail

git fetch --all
git merge --ff-only
git push origin
git push wiki
