# ~/.bash_profile
# George Whiteside

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Start an ssh-agent.
eval $(ssh-agent)

# Add my bin to path.
PATH=$PATH:$HOME/bin

# Set my default editor to emacs.
EDITOR=emacs

# Set my default less options--turn off bell and ignore case in search.
LESS='Qi'

export PATH EDITOR LESS
