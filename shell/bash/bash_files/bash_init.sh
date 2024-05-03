#!/bin/bash

export EDITOR='emacs'
export FIFUNC=$HOME/git/Config-files
export COMMON_BASH_DIR=$HOME/.config/common_bash

# Source common alias betwee fish and bash
source $COMMON_BASH_DIR/alias.sh
source $FIFUNC/functions.sh
