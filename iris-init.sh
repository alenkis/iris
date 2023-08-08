#!/bin/bash

# Exit script on any failure
set -e

# Define the URL of the release
RELEASE_URL="https://github.com/alenkis/iris/releases/download/v0.1.0.0/iris-0.1.0.0.tar.gz"

# Download the release
TEMP_DIR=$(mktemp -d)
curl -L "$RELEASE_URL" -o "$TEMP_DIR/iris.tar.gz"

# Unpack the release
tar -xzf "$TEMP_DIR/iris.tar.gz" -C "$TEMP_DIR"

# Check if iris is in a sub-directory
IRIS_PATH=$(tar -tzf "$TEMP_DIR/iris.tar.gz" | grep "/iris$" || echo "iris")

# Ensure there is a local bin directory in HOME and add to PATH
LOCAL_BIN="$HOME/.local/bin"
mkdir -p "$LOCAL_BIN"
echo "export PATH=\"$LOCAL_BIN:\$PATH\"" >> "$HOME/.bashrc"

# Move the iris binary to the local bin directory
mv "$TEMP_DIR/$IRIS_PATH" "$LOCAL_BIN"

# Clean up
rm -r "$TEMP_DIR"

echo "Installation completed! Please restart your terminal or run 'source ~/.bashrc'."
