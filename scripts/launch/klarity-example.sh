#!/usr/bin/env sh

SCRIPT=$(readlink -f "$0")
SCRIPT_PATH=$(dirname "$SCRIPT")

NODE_TLS_REJECT_UNAUTHORIZED=0 "$SCRIPT_PATH/../../klarity/release/linux-x64/Bezalel-linux-x64/Bezalel"
