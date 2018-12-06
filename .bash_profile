# .bash_profile
# George Whiteside

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# add my bin to path
PATH=$PATH:$HOME/bin

# set default editor to emacs
EDITOR=emacs

export PATH EDITOR
