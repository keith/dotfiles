#!/usr/bin/env python3

from datetime import datetime, timedelta, timezone
import argparse
import json
import subprocess
import sys
import time


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repo",
        type=str,
        help="The github repository in the format 'owner/repo'.",
    )
    parser.add_argument(
        "workflows",
        type=str,
        nargs="+",
        help="The basename of the workflow files to delete 'foo.yaml'",
    )
    return parser


def _list_workflow_runs(repo: str, workflow: str) -> list[str]:
    result = subprocess.run(
        [
            "gh",
            "run",
            "list",
            "--repo",
            repo,
            "--workflow",
            workflow,
            "--json",
            "databaseId",
            "--limit",
            "500",  # Avoid setting something too high and waste all the limit fetching without deleting anything
        ],
        text=True,
        check=False,
        capture_output=True,
    )

    if result.returncode != 0:
        if "HTTP 404" in result.stderr:
            print(f"Workflow '{workflow}' not found in repository '{repo}'.")
            return []
        raise subprocess.CalledProcessError(
            result.returncode,
            result.args,
            output=result.stdout,
            stderr=result.stderr,
        )

    return [str(x["databaseId"]) for x in json.loads(result.stdout)]


def _delete_workflow_runs(repo: str, workflow: str) -> bool:
    try:
        while True:
            ids = _list_workflow_runs(repo, workflow)
            if not ids:
                print(f"No more workflow runs found for {workflow} in {repo}.")
                return True

            for run_id in ids:
                print(f"Deleting workflow run {run_id} for {workflow} in {repo}...")
                subprocess.check_call(["gh", "run", "delete", "--repo", repo, run_id])

        return True
    except subprocess.CalledProcessError as e:
        print(f"Delete failed: {e}")
        return False


# % gh api rate_limit | jq '.resources.core'
# {
#   "limit": 5000,
#   "used": 4880,
#   "remaining": 120,
#   "reset": 1750294544
# }
def _wait_for_rate_limit() -> None:
    output = subprocess.check_output(["gh", "api", "rate_limit"], text=True)
    rate_limit = json.loads(output)
    limit_info = rate_limit["resources"]["core"]
    if limit_info["remaining"] > 0:
        print("Rate limit is okay, remaining:", limit_info["remaining"])
        return  # Assume a 500 or something

    reset_time = datetime.fromtimestamp(limit_info["reset"], tz=timezone.utc)
    now = datetime.now(tz=timezone.utc)
    difference = reset_time - datetime.now(tz=timezone.utc)
    wait_seconds = difference.seconds
    print(
        f"Rate limit exceeded. Waiting for {wait_seconds / 60} minutes, until {reset_time.astimezone().time()}"
    )

    # Add time to ensure we don't hit the limit again immediately
    for remaining in range(wait_seconds + 5, 0, -1):
        print(
            "{} remaining...".format(timedelta(seconds=remaining)), end="\r", flush=True
        )
        time.sleep(1)

    print("Rate limit reset, continuing...")


def _main(repo: str, workflows: list[str]) -> None:
    workflow = workflows.pop(0)
    while workflow:
        print(f"Deleting workflow runs for {workflow} in {repo}...")
        success = _delete_workflow_runs(repo, workflow)
        if success:
            print(f"Successfully deleted workflow runs for {workflow}.")
            if workflows:
                workflow = workflows.pop(0)
            else:
                print("All specified workflows have been deleted!")
                break
        else:
            print(f"Failed to delete workflow runs for {workflow}.")
            _wait_for_rate_limit()


if __name__ == "__main__":
    parser = _build_parser()
    args = parser.parse_args()
    _main(args.repo, sorted(set(args.workflows)))
