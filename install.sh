#!/bin/bash

#===============================================================================
# TODO:
# - ask on overwrite, give full information
#===============================================================================

while [[ $# > 0 ]]; do
    key="$1"
    case "$key" in
    -q|--quick)
        QUICK="1"
        shift
        ;;
    *)
        shift
        ;;
    esac
done

# Pull down latest git-submodules
if [[ ! "$QUICK" ]]; then
    echo "Updating third-party repositories...\n"
    git submodule foreach git pull origin master
    echo "\nFinished updating third-party repositories.\n"
fi

# Create the symbolic links
dotfiles=(\
    vim\
    vimrc\
    xmonad\
    Xresources\
    zprofile\
    zshrc\
    zshrc_alias\
)

INSTALL_DIR="${HOME}/test_home"

for fd in ${dotfiles[@]}; do
    ln -sfv "$(pwd)/${fd}" "${INSTALL_DIR}/.${fd}"
done

# Link up common directories
directories=(bin)
for fd in ${directories[@]}; do
    ln -sfv "$(pwd)/${fd}" "${INSTALL_DIR}/"
done

# Deal with special cases
ln -sfv lib/oh-my-zsh "${INSTALL_DIR}/.oh-my.zsh"

mkdir -p "${INSTALL_DIR}/.oh-my-zsh/custom/themes"
ln -sfv xathereal.zsh-theme "${INSTALL_DIR}/.oh-my-zsh/custom/themes/xathereal.zsh-theme"
