#!/bin/sh

cabal sandbox init

cabal sandbox add-source ../snap-loader-static/
cabal sandbox add-source ../snap
cabal sandbox add-source ../snap-server
cabal sandbox add-source ../snap-core
cabal sandbox add-source ../io-streams-haproxy/
cabal sandbox add-source ../heist/
cabal sandbox add-source ../xmlhtml/
cabal sandbox add-source ../snaplet-persistent/
