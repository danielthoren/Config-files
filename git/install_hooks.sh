#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo "Installing 'TODO' pre-commit hook"
echo "$workingDir/hooks/todo_warning-pre-commit-hook.sh" >> .git/hooks/pre-commit
chmod +rwx .git/hooks/pre-commit
