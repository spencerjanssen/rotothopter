#!/bin/sh
cabal2nix . --no-haddock --no-check > rotothopter.nix
