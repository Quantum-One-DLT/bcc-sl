#!/usr/bin/env bash

# clear old data, don't remove the databases since they may contains some data
# that will help speed up the syncing process
rm -rf ./run/* ./node-* ./*key* ./*.dump

stack exec -- bcc-explorer \
    --system-start 1499246772 \
    --log-config log-config-prod.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --kademlia-peer bcc-node-0.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-1.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-2.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-3.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-4.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-5.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-6.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-7.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-8.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-9.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-10.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-11.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-12.aws.blockchain-company.io:3000 \
    --kademlia-peer bcc-node-13.aws.blockchain-company.io:3000 \
    --listen 127.0.0.1:$((3000)) \
    --static-peers \
    "$@"
