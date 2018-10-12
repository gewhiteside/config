# ~/.bashrc
# Sets up my preferred bash environment

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

# append to the history file, don't overwrite it
shopt -s histappend
# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# don't try to complete an empty command
shopt -s no_empty_cmd_completion

# set color prompt
PS1='\e[1;37m\u@\h:\w\$\e[m '

# colors for ls and grep
# ignore backup files in ls and grep
eval "$(dircolors -b)"
alias ls='ls --color=auto --hide="*~" --hide="*#"'
alias la='ls -A --color=auto'
alias grep='grep --color=auto --exclude=*~ --exclude=*#'
alias fgrep='fgrep --color=auto --exclude=*~ --exclude=*#'
alias egrep='egrep --color=auto --exclude=*~ --exclude=*#'

# aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

function nscreen() {
    # TODO: add ability to dispaly this title when reconnecting
    echo -ne "\e]0;$(whoami)@$(hostname) | screen: $1\a"
    screen -S $1
}

# start ssh agent and kill it on exit
function ssh-start() {
    eval $(ssh-agent)
    ssh-add
    trap "kill $SSH_AGENT_PID" EXIT
}
