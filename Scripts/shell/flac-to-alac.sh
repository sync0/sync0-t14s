#!/usr/bin/env bash
my_bin="$(dirname $0)/flac-to-alac-ffmpeg.sh"
find . -type f -name '*.flac' -exec "$my_bin" {} \;
