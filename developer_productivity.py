"""developer_productivity.py - GitHub/Git Stats Checker

This script fetches and displays developer productivity statistics from either the local
Git log or a GitHub repository, controlled by a command-line flag.

Author: Jason Walsh <j@wal.sh>
Date: 2024-08-01
"""

import os
from datetime import datetime, timedelta
from typing import List, Optional

import click
import requests
from git import Repo  # Using GitPython


# Configuration
DEFAULT_GIT_REPO_PATH = "."        # Current directory
DEFAULT_SINCE = timedelta(weeks=1)  # Look back 1 week by default

def get_commit_activity(repo_path: str, since: timedelta) -> int:
    """Fetches the number of commits in a Git repository since a given time.

    Args:
        repo_path: Path to the Git repository.
        since: Timedelta specifying how far back to look for commits.

    Returns:
        The number of commits made since the specified time.
    """
    repo = Repo(repo_path)
    commits = list(repo.iter_commits(since=since))
    return len(commits)


def get_github_commit_activity(owner: str, repo: str) -> int:
    """Fetches commit activity for the past week from a GitHub repository.

    Args:
        owner: GitHub repository owner.
        repo: GitHub repository name.

    Returns:
        The number of commits made in the past week.

    Raises:
        ValueError: If the GITHUB_TOKEN environment variable is not set
            or if the GitHub API request fails.
    """
    github_token = os.getenv("GITHUB_TOKEN")
    if not github_token:
        raise ValueError("GITHUB_TOKEN environment variable not set")

    url = f"https://api.github.com/repos/{owner}/{repo}/commits?per_page=100"
    headers = {"Authorization": f"Bearer {github_token}"}
    response = requests.get(url, headers=headers)
    response.raise_for_status()

    commits = response.json()
    since = datetime.now() - timedelta(weeks=1)
    return len([commit for commit in commits if datetime.fromisoformat(commit["commit"]["author"]["date"][:-1]) > since])  # GitHub returns timestamps in ISO 8601


@click.command()
@click.option("--github", is_flag=True, help="Check GitHub instead of local Git log")
@click.option("--repo-path", type=click.Path(exists=True), default=DEFAULT_GIT_REPO_PATH,
              help="Path to the local Git repository (default: current directory)")
@click.option("--since", type=click.IntRange(min=0), default=DEFAULT_SINCE.days,
              help="Number of days to look back for commits (default: 7)")
@click.argument("owner", required=False)  # GitHub owner (only if --github is used)
@click.argument("repo", required=False)   # GitHub repo (only if --github is used)
def main(github: bool, repo_path: str, since: int, owner: Optional[str], repo: Optional[str]):
    """Main function."""
    if github:
        if not owner or not repo:
            raise click.UsageError("Owner and repo are required when using --github")
        commits = get_github_commit_activity(owner, repo)
    else:
        commits = get_commit_activity(repo_path, timedelta(days=since))
    click.echo(f"Commits in the past {since} days: {commits}")

if __name__ == "__main__":
    main()
