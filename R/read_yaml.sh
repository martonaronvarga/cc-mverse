#!/usr/bin/env bash
# read_yaml.sh — Minimal YAML value reader for flat/nested keys

yaml_get() {
  local file="$1" keypath="$2"

  # Pass file and keypath as environment variables to avoid any quoting issues.
  YAML_FILE="$file" YAML_KEY="$keypath" \
  ~/local/python3.11.0/bin/python3 - << 'PYEOF'
import yaml, os, sys, functools, operator
file    = os.environ["YAML_FILE"]
keypath = os.environ["YAML_KEY"]
with open(file) as f:
    d = yaml.safe_load(f)
keys = keypath.split('.')
try:
    print(functools.reduce(operator.getitem, keys, d))
except (KeyError, TypeError):
    sys.exit(1)
PYEOF
  return $?
}
