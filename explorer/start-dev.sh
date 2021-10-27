#!/usr/bin/env bash

# Remove logs
rm ./*.log

# Optional path for `bcc-sl`
bcc_path=${1:-../}

date=$(date +%s)
export system_start=$((date + 1))

./scripts/run.sh "$bcc_path"/scripts/common-functions.sh & PIDEX=$!
WALLET_TEST=1 "$bcc_path"/scripts/launch/demo-with-wallet-api.sh & PIDNODE=$!

wait $PIDEX
wait $PIDNODE
