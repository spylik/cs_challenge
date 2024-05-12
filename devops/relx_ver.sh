#!/bin/sh
set -e

(test -n "${GIT_REVISION}" && printf $GIT_REVISION) || printf $(git rev-parse --short HEAD)
