set PATH ~/.local/bin $PATH

export EDITOR='emacs'
export FIFUNC='/home/'$USER'/git/Config-files'
export COMMON_BASH_DIR="$HOME/.config/common_bash"
export FISH_FUNC_DIR="$HOME/.config/fish/functions"

# Source common alias betwee fish and bash
source $COMMON_BASH_DIR/alias.sh
