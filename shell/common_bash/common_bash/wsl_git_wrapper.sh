#!/bin/bash

# checks to see if we are in a windows or linux dir
function isWinDir {
    case $PWD/ in
        /mnt/*)
            return $(true)
            ;;
        *)
            return $(false)
            ;;
    esac
}

# wrap the git command to either run windows git or linux
function git {
  if isWinDir
  then
      echo "Windows git"
      git.exe "$@"
  else
      echo "Linux git"
      /usr/bin/git "$@"
  fi
}
