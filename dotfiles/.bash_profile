# ~/.bash_profile
# George Whiteside

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Check for an ssh-agent. If there isn't one, start one.
# TODO: What happens if I am running more than one ssh-agent?
SSH_AGENT_PID=$(pgrep -u $USER ssh-agent)
if [ $SSH_AGENT_PID ]; then
    export SSH_AGENT_PID
    export SSH_AUTH_SOCK=~/.ssh/ssh-agent-socket
else
    eval $(ssh-agent -a ~/.ssh/ssh-agent-socket)
fi

# Add my bin to path.
PATH=$PATH:$HOME/bin

# Set my default editor to emacs. Don't set desktop-save mode for these emacs
# sessions.
EDITOR='emacs --no-desktop'

# Set my default less options--turn off bell and ignore case in search.
LESS='Qi'

export PATH EDITOR LESS
