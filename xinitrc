#!/bin/sh

# exports for everyone (namely XMonad)
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.bin
export PATH=$PATH:$HOME/.bin-laptop
export PATH=$PATH:$HOME/.bin-freelancer

# ensure that pulseaudio is running
killall pulseaudio; pulseaudio &

# set the cursor theme
xsetroot -cursor_name left_ptr

# run xrandr settings
~/.screenlayout/main.sh

# restore the wallpaper
nitrogen --restore

# custom key mappings
xmodmap ~/.xmodmaprc

# load X settings
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
urxvtd -q -o -f &

[[ -f ~/.config/comtpon.conf ]] && compton -b --config ~/.config/compton.conf

xscreensaver -no-splash &

# start the window manager
exec xmonad
