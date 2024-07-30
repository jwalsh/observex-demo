#!/bin/bash

# Function to check if a file is ignored by git
is_ignored_by_git() {
    if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        git check-ignore -q "$1"
    else
        return 1  # Not a git repository, so nothing is ignored
    fi
}

# Check if a directory argument is provided
if [ $# -eq 1 ] && [ -d "$1" ]; then
    search_dir="$1"
else
    search_dir="."
fi

echo "Searching in directory: $search_dir"

# Find all files in the specified directory
find "$search_dir" -type f | while read -r file; do
    # Check if the file is a text file
    if file -b --mime-type "$file" | grep -q "^text/"; then
        # Check if the file is not ignored by git (if in a git repo)
        if ! is_ignored_by_git "$file"; then
            echo "=== File: $file ==="
            echo "----------------------------------------"
            if [ -s "$file" ]; then
                cat "$file"
            else
                echo "File is empty"
            fi
            echo "----------------------------------------"
            echo ""
        else
            echo "Skipping git-ignored file: $file"
        fi
    else
        echo "Skipping non-text file: $file"
    fi
done

echo "Search completed."