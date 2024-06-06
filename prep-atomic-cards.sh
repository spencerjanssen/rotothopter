#!/usr/bin/env bash
jq -c '{data: .data | with_entries({key: .key, value: .value | map({name: .name, faceName: .faceName, colors: .colors, types: .types, layout: .layout})}) }' < AtomicCards.json > AtomicCards.min.json
