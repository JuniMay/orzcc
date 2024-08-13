#!/bin/bash

# Directory containing the .sy files
sy_directory="./tests/sysy/external_test"



# Base command to execute
command_base="python3 ./scripts/test_oracle.py"

echo "Running test_oracle for external tests (opt level 0)"
echo ""
# Iterate over .sy files in the directory
for file in "$sy_directory"/*.sy; do
    # Execute the command with the current file as an argument
    $command_base "$file" --timeout 600 --opt-level 0 --output-dir ./output --executable-path ./target/release/compiler --no-compile
done

echo "Running test_oracle for external tests (opt level 1)"
echo ""
# Iterate over .sy files in the directory
for file in "$sy_directory"/*.sy; do
    # Execute the command with the current file as an argument
    $command_base "$file" --timeout 600 --opt-level 1 --output-dir ./output --executable-path ./target/release/compiler --no-compile
done