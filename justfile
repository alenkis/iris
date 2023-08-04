default:
    @just --choose

run *args:
    @cabal run -- iris {{args}}

help:
    @just run --help

version:
    @just run --version

transform:
    @just run transform -c examples/simple.toml -f examples/simple.txt -o examples/simple.out.csv

ssense:
    @just run transform -c examples/ssense.toml -f examples/ssense.txt -o examples/ssense.out.csv

gen-hie:
    @gen-hie > hie.yaml

build-docker:
    @docker build -t iris:latest .
    @docker create --name iris-container iris:latest
    @docker cp iris-container:/usr/local/bin/iris .
    @docker rm iris-container
