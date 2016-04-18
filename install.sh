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
    bashrc\
    bash_profile\
    conkyrc\
    conky_dzen\
    dir_colors\
    dzen_icons\
    gitconfig\
    tmux.conf\
    vim\
    vimrc\
    xinitrc\
    xmodmaprc\
    xmonad\
    Xresources\
    zprofile\
    zshrc\
    zshrc_alias\
)

INSTALL_DIR="${HOME}"

for fd in ${dotfiles[@]}; do
    ln -sfv -T "$(pwd)/${fd}" "${INSTALL_DIR}/.${fd}"
done

# Link up common directories
directories=(bin etc theme)
for fd in ${directories[@]}; do
    ln -sfv "$(pwd)/${fd}" "${INSTALL_DIR}/"
done

# Deal with special cases
ln -sfv "$(pwd)/lib/oh-my-zsh" "${INSTALL_DIR}/.oh-my-zsh"

mkdir -p "${INSTALL_DIR}/.oh-my-zsh/custom/themes"
ln -sfv "$(pwd)/xathereal.zsh-theme" "${INSTALL_DIR}/.oh-my-zsh/custom/themes/xathereal.zsh-theme"

# vim stuff

specific="\
    lib/base16-vim/colors/base16-tomorrow.vim .vim/colors/base16-tomorrow.vim\
    lib/base16-vim .vim/bundle/base16-vim\
    lib/nerdtree .vim/bundle/nerdtree\
    lib/vim-better-whitespace .vim/bundle/vim-better-whitespace\
    lib/vim-pathogen/autoload/pathogen.vim .vim/autoload/pathogen.vim\
"

set -- $specific
while [ ! -z "$1" ]; do
    #echo "($(pwd)/$1, ${INSTALL_DIR}/$2)"
    ln -sfv\
        "$(pwd)/${1}"\
        "${INSTALL_DIR}/${2}"
    shift 2
done



