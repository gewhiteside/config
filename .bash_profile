# .bash_profile
# George Whiteside

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# add my bin to path
PATH=$PATH:$HOME/bin

# set default editor to emacs
EDITOR=emacs

# default less options--turn off bell and ignore case in search
LESS='Qi'

export PATH EDITOR LESS
