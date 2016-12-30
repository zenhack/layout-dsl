#!/usr/bin/env sh
set -ex
cabal configure --enable-tests
cabal build
cabal test
