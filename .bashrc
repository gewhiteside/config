# ~/.bashrc
# George Whiteside

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

##### GENERAL #####
## FUNCTIONS ##
# open emacs in a new screen session called emacs, or reattach to one if it
# already exists
function screenmacs {
    set-title "screenmacs"
    screen -dR emacs emacs
    reset-title
}

# opens most recent todo list
function open-notes {
    set-title "notes"
    cd ~/org/work/notes
    emacs $(ls -1 *.org --hide="*~" | sort -r | head -n 1)
    reset-title
}

# start ssh agent and kill it on exit
function ssh-start {
    eval $(ssh-agent)
    ssh-add
    trap "kill $SSH_AGENT_PID" EXIT
}

# resize window to default 80x24
function resize {
    printf '\e[8;24;80t'
}

## DEFINITIONS ##
# append to the history file, don't overwrite it
shopt -s histappend
# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# don't try to complete an empty command
shopt -s no_empty_cmd_completion

# colors for ls
eval "$(dircolors -b)"

# aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# set C-x-e editor to emacs
export EDITOR=emacs


##### PROMPT #####
## FUNCTIONS ##
# update PS1 with current title and prompt color
function update-ps1 { PS1="$TITLE$PROMPT" ;}

# reset PS1 to default
function reset-ps1 {
    reset-title
    reset-color
    update-ps1
}

# sets static title
function set-title {
    TITLE=
    printf "\e]0;$(whoami)@$(hostname): $1\a"
    update-ps1
}

# resets behaivor to set title every prompt
function reset-title {
    TITLE="$DEFAULT_TITLE"
    update-ps1
}

# set, reset and clear color prompt
function set-color {
    COLOR="$1"
    update-ps1
}
function reset-color {
    COLOR="$DEFAULT_COLOR"
    update-ps1
}
function clear-color {
    COLOR="0m"
    update-ps1
}

## DEFINITIONS ##
DEFAULT_TITLE='\[\e]0;\u@\h:\w\a\]'
TITLE="$DEFAULT_TITLE"

BASE_PROMPT="\u@\h:\w"

NEWLINE_IF_LONG='\[$(if [ ${#PWD} -gt $LONG_PWD ]; then printf "\]\n\["; fi)\]'
SUFFIX="$NEWLINE_IF_LONG\$ "

# cutoff for a long pwd
LONG_PWD=40

# keep track of default and current prompt color
DEFAULT_COLOR="0m"
COLOR="$DEFAULT_COLOR"

PROMPT="\[\e[\$COLOR\]$BASE_PROMPT$SUFFIX\[\e[0m\]"

# initialize PS1
update-ps1
