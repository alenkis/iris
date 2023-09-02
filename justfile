default:
    @just --choose

run *args:
    @cabal run -- iris {{args}}

help:
    @just run --help

version:
    @just run --version

test:
    @cabal test --test-option=--color

transform *args:
    @just run transform {{args}}

gen-hie:
    @gen-hie > hie.yaml
