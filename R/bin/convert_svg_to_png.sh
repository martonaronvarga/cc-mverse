#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec Rscript "${script_dir}/convert_svg_to_png.R" "$@"
