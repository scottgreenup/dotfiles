#-------------------------------------------------------------------------------
# ZSH Settings / Theming
#-------------------------------------------------------------------------------
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="xathereal"
COMPLETION_WAITING_DOTS="true"
DISABLE_CORRECTION="true"

plugins=(archlinux git git-flow)

source $ZSH/oh-my-zsh.sh

# Run color theme for everything
BASE16_SHELL="$HOME/.config/base16-shell/base16-tomorrow.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

#-------------------------------------------------------------------------------
# Exports
#-------------------------------------------------------------------------------

# For using Arcanist at Freelancer
export PATH=$PATH:$HOME/phabricator/arcanist/bin

# Disable virutalenv prompt
export VIRTUAL_ENV_DISABLE_PROMPT="goaway"

# Ensure the term and editor are correct
export TERM='rxvt-256color'
export EDITOR='vim'

#-------------------------------------------------------------------------------
# Other Settings
#-------------------------------------------------------------------------------

# Enable extended globbing
setopt extended_glob

bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line

# Source in all aliases
source "$HOME/.zshrc_alias"

# change directory colours
eval $(dircolors -b ~/.dir_colors)
xrdb $HOME/.Xresources

# session based ssh key
#eval `keychain --eval --agents ssh id_rsa --timeout 600 --quiet`
