#!/usr/bin/env bash

# developer_productivity.sh - GitHub/Git Stats Checker

# Description:
#   Fetches and displays developer productivity statistics from either the local
#   Git log or a GitHub repository, controlled by a command-line flag.

# Author: Jason Walsh <j@wal.sh>
# Date: 2024-08-01

set -euo pipefail

# Configuration
DEFAULT_USE_GITHUB=false   # Default: Check local Git log
DEFAULT_GIT_REPO="."       # Current directory
DEFAULT_SINCE="1 week ago"

USE_GITHUB=$DEFAULT_USE_GITHUB

# Argument parsing
while [[ $# -gt 0 ]]; do
  case "$1" in
    --github)
      USE_GITHUB=true
      shift
      ;;
    *)
      echo "Invalid argument: $1" >&2
      exit 1
      ;;
  esac
done

# GitHub API function
get_github_commit_activity() {
  local owner="$1"
  local repo="$2"
  local github_token="${GITHUB_TOKEN}"

  if [[ -z "$github_token" ]]; then
    echo "GITHUB_TOKEN environment variable not set" >&2
    exit 1
  fi

  local url="https://api.github.com/repos/$owner/$repo/commits?per_page=100"
  local response=$(curl -s -H "Authorization: Bearer $github_token" "$url")
  if [[ $? -ne 0 ]]; then
    echo "GitHub API request failed" >&2
    exit 1
  fi

  echo "$response" | jq '.[] | select(.commit.author.date > "2024-07-25T08:52:30Z")' | jq -s 'length'
}

# Git log function
get_git_log_commit_activity() {
  local repo_path="$1"
  local since="$2"

  git -C "$repo_path" log --since="$since" --oneline | wc -l
}


main() {
    local owner=""  # Initialize as empty strings
    local repo=""
    local commits=0

    if [[ $USE_GITHUB == true ]]; then
        if [[ $# -lt 2 ]]; then  # Check if at least 2 arguments are provided
            echo "Owner and repo are required when using --github" >&2
            exit 1
        fi
        owner="$1"  # Assign arguments only if they exist
        repo="$2"
        commits=$(get_github_commit_activity "$owner" "$repo")
    else
        commits=$(get_git_log_commit_activity "$DEFAULT_GIT_REPO" "$DEFAULT_SINCE")
    fi

    echo "Commits in the past week: $commits"
}

main "$@"  # Still pass all arguments to main
