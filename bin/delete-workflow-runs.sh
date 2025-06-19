#!/usr/bin/env bash

# https://docs.github.com/en/actions/monitoring-and-troubleshooting-workflows/monitoring-workflows/using-workflow-run-logs#example-script
# Delete all logs for a given workflow
# Usage: delete-logs.sh <repository> <workflow-name>

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <repository> <workflow-name>" >&2
  exit 1
fi

REPOSITORY=$1
WORKFLOW_NAME=$2

echo "Getting all completed runs for workflow $WORKFLOW_NAME in $REPOSITORY"

RUNS=$(
  gh api \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "/repos/$REPOSITORY/actions/workflows/$WORKFLOW_NAME/runs" \
    --paginate \
    --jq '.workflow_runs[] | select(.conclusion != "") | .id'
)

echo "Found $(echo "$RUNS" | wc -l) completed runs for workflow $WORKFLOW_NAME"

for RUN in $RUNS; do
  gh run delete --repo "$REPOSITORY" "$RUN"
done
