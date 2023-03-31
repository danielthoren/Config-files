#!/bin/bash

userName="danielthoren"
userEmail="danne_thoren456@hotmail.com"

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if ! $(git config user.name | grep -q $userName);
then
    echo "Setting git config.name to: " $userName
    git config user.name $userName
fi

if ! $(git config user.email | grep -q $userEmail);
then
    echo "Setting git config.email to: " $userEmail
    git config user.email $userEmail
fi

if git submodule status | grep --quiet '^-'; then
    echo "Initializing submodules..."
    git submodule update --init --recursive
fi

bash "$workingDir/emacs/emacs_files/Block-Comment-Mode/init_repo.sh"
