# ~/.bashrc
# George Whiteside

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

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

# prompt which updates window title
TITLE_PROMPT='\[\e]0;\u@\h:\w\a\e[1;37m\]\u@\h:\w\$\[\e[0m\] '
# prompt which doesn't update window title
PROMPT='\[\e[1;37m\]\u@\h:\w\$\[\e[0m\] '
# set color prompt and title
PS1=$TITLE_PROMPT

# set C-x-e editor to emacs
EDITOR=emacs

# sets static title
function title() {
    PS1=$PROMPT
    echo -ne "\033]0;$(whoami)@$(hostname): $1\a"
}
# resets behaivor to set title every prompt
function clear-title() {
    PS1=$TITLE_PROMPT
}

# open emacs in a new screen session called emacs, or reattach to one if it
# already exists
function screenmacs() {
    title "screenmacs"
    screen -dR emacs emacs
    clear-title
}

# opens most recent todo list
function open-notes() {
    title "notes"
    cd ~/org/work/notes
    emacs $(ls -1 *.org --hide="*~" | sort -r | head -n 1)
    clear-title
}

# start ssh agent and kill it on exit
function ssh-start() {
    eval $(ssh-agent)
    ssh-add
    trap "kill $SSH_AGENT_PID" EXIT
}
