#!/usr/bin/env sh

if [ $1 = "build" ]; then
    stack build
elif [ $1 = "test" ]; then
    stack run test run
else
    echo "Invalid command."
fi
