set PATH ~/.local/bin $PATH

export EDITOR='emacs'
export FIFUNC='/home/'$USER'/git/Config-files'
export COMMON_BASH_DIR="/$HOME/.config/common_bash"

# Source common alias betwee fish and bash
source $COMMON_BASH_DIR/alias.sh

source ~/.config/fish/functions/wsl_config.fish
