# Stage 1: Build the application
FROM haskell:9.4.5 as builder

# Set up working directory
WORKDIR /build

# Copy the cabal files and resolve dependencies
COPY ./iris.cabal ./stack.yaml* ./hie.yaml /build/
COPY ./docker.cabal.config /build/cabal.config

CMD ["ls -la"]

RUN cabal update

# Build the application
COPY . /build
RUN cabal build

# Find the location of the built executable
RUN mkdir -p /artifacts
RUN find /build/dist-newstyle -type f -executable -name iris -exec cp {} /artifacts \;

# Stage 2: Copy the built application into a new image
FROM ubuntu:20.04

# Copy the built application from the builder stage
COPY --from=builder /artifacts/iris /usr/local/bin

# Set the entry point to the application executable
ENTRYPOINT ["/usr/local/bin/iris"]
