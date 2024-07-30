#!/bin/bash

# Function to check if a file is ignored by git
is_ignored_by_git() {
    git check-ignore -q "$1"
}

# Find all files, excluding those ignored by git
git ls-files --others --exclude-standard -z 2>/dev/null && git ls-files -z 2>/dev/null | sort -zu | while IFS= read -r -d '' file; do
    # Check if the file is a text file
    if file -b --mime-type "$file" | grep -q "^text/"; then
        # Check if the file is not ignored by git
        if ! is_ignored_by_git "$file"; then
            echo "=== File: $file ==="
            echo "----------------------------------------"
            cat "$file"
            echo "----------------------------------------"
            echo ""
        fi
    fi
done