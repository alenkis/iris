default:
    @just --choose

run *args:
    @cabal run -- iris {{args}}

help:
    @just run --help

version:
    @just run --version

transform:
    @just run transform -c examples/simple.toml -f examples/simple.txt

ssense:
    @just run transform -c examples/ssense.toml -f examples/ssense.txt

gen-hie:
    @gen-hie > hie.yaml
