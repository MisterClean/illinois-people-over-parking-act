#!/usr/bin/env bash
set -euo pipefail

# Bootstrap R environment and required packages for the analysis.
# - Installs R via apt-get on Debian/Ubuntu if it is missing.
# - Installs CRAN packages used by the notebook.

APT_GET_CMD="apt-get"
SUDO_CMD=""

command -v sudo >/dev/null 2>&1 && SUDO_CMD="sudo "

if ! command -v R >/dev/null 2>&1; then
  if ! command -v ${APT_GET_CMD} >/dev/null 2>&1; then
    echo "apt-get is required to install R automatically. Please install R manually." >&2
    exit 1
  fi

  echo "R not found. Installing via apt-get..."
  ${SUDO_CMD}${APT_GET_CMD} update
  ${SUDO_CMD}${APT_GET_CMD} install -y r-base
else
  echo "R is already installed; skipping R installation."
fi

missing_pkgs=$(Rscript - <<'RSCRIPT'
pkgs <- c(
  "tidyverse",
  "sf",
  "lwgeom",
  "leaflet",
  "leaflet.extras",
  "data.table",
  "zip",
  "httr",
  "lubridate",
  "mapview",
  "tigris",
  "kableExtra",
  "tidytransit"
)
installed <- rownames(installed.packages())
missing <- pkgs[!(pkgs %in% installed)]
cat(paste(missing, collapse = " "))
RSCRIPT
)

if [ -n "$missing_pkgs" ]; then
  echo "Installing missing R packages: $missing_pkgs"
  Rscript -e "install.packages(strsplit('$missing_pkgs', ' ')[[1]], repos='https://cloud.r-project.org')"
else
  echo "All required R packages are already installed; skipping package installation."
fi

echo "R environment bootstrap complete."
