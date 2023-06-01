#!/bin/sh

read -p "Enter old email (Pattern matching works): " old_email
read -p "Enter new email: " new_email
read -p "Enter new name: " new_name

git filter-branch --env-filter '

if [ "$GIT_COMMITTER_EMAIL" =~ "$old_email" ]
then
    export GIT_COMMITTER_NAME="$new_name"
    export GIT_COMMITTER_EMAIL="$new_email"
fi
if [ "$GIT_AUTHOR_EMAIL" =~ "$old_email" ]
then
    export GIT_AUTHOR_NAME="$new_name"
    export GIT_AUTHOR_EMAIL="$new_email"
fi
' --tag-name-filter cat -- --branches --tags
