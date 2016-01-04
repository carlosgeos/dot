#
# ~/.bash_profile
#

# get .bashrc! see man bash for explanation on .bash_profile and .bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc


# start X
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
