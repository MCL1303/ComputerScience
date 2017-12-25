#!/bin/bash
set -eux -o pipefail

git fetch --all
git push origin
git push wiki
