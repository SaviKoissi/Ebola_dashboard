#!/bin/bash

#=============================================================================
# automate_pipeline.sh
# Ebola 2026 Project - Portable Automated Ingestion Framework
# Written by Koissi Savi
#=============================================================================

# Exit immediately if a command exits with a non-zero status
set -e

# Dynamically locate the directory where THIS script is physically saved
# This allows the pipeline to find files relatively, no matter where it is executed from
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Move safely into the project root directory
cd "$SCRIPT_DIR"

# Define relative path coordinates matching your project structure
LOG_FILE="logs/cron_pipeline.log"
DATA_FILE="data/incoming/ebola.csv"

# Ensure runtime directory frameworks exist locally
mkdir -p logs data/incoming

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 🚀 Starting automated relative ingestion cascade..." >> "$LOG_FILE"

#---------------------------------------------------------------------------
# Step 1: Execute Primary Web Scraping Pipeline (CDC Live & Historical)
#---------------------------------------------------------------------------
if [ -f "scripts/fetch_ebola_data.R" ]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] -> Executing primary CDC web miner..." >> "$LOG_FILE"
    Rscript scripts/fetch_ebola_data.R >> "$LOG_FILE" 2>&1
else
    echo "[ERROR] scripts/fetch_ebola_data.R not found in $SCRIPT_DIR/scripts/" >> "$LOG_FILE"
    exit 1
fi

#---------------------------------------------------------------------------
# Step 2: Execute Alternative Web Mining Framework (WHO DON & HDX APIs)
#---------------------------------------------------------------------------
if [ -f "scripts/fetch_alternative_sources.R" ]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] -> Syncing alternative WHO & HDX pipelines..." >> "$LOG_FILE"
    Rscript scripts/fetch_alternative_sources.R >> "$LOG_FILE" 2>&1
else
    echo "[ERROR] scripts/fetch_alternative_sources.R not found in $SCRIPT_DIR/scripts/" >> "$LOG_FILE"
    exit 1
fi

#---------------------------------------------------------------------------
# Step 3: Evict Cache & Force Shiny App Hot-Reload
#---------------------------------------------------------------------------
if [ -f "$DATA_FILE" ]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] -> Refreshing mtime to trigger Shiny reactivePoll..." >> "$LOG_FILE"
    
    # Touch updates the file timestamp, signaling valueFunc to update the frontend
    touch "$DATA_FILE"
    
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] ✔ Pipeline completed successfully." >> "$LOG_FILE"
else
    echo "[WARNING] Target database file ($DATA_FILE) was not found or modified." >> "$LOG_FILE"
fi

exit 0