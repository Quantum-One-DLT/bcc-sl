#!/usr/bin/env bash

set -euo pipefail


exec "$(nix-build "$(dirname "$0")/tbco-common.nix" -A nix-tools.regeneratePackages --no-out-link --option substituters "https://hydra.blockchain-company.io https://cache.nixos.org" --option trusted-substituters "" --option trusted-public-keys "hydra.blockchain-company.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=")"
