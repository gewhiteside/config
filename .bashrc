# ~/.bashrc
# George Whiteside

# if shell is not interactive, don't do anything
[[ $- != *i* ]] && return

##### GENERAL #####
# Open emacs in a new screen session called emacs, or reattach to one if it
# already exists.
screenmacs()
{
    set_title screenmacs
    screen -dR emacs emacs
    reset_title
}

# same as above for a shell
screenshell()
{
    set_title screenshell
    screen -dR shell
    reset_title
}

# open most recent todo list in emacs
open_notes()
{
    set_title notes
    cd ~/org/work/notes
    emacs $(ls -1 *.org --hide="*~" | sort -r | head -n 1)
    reset_title
}

# start ssh-agent and kill it on exit
ssh_start()
{
    eval $(ssh-agent)
    ssh-add
    trap "kill $SSH_AGENT_PID" EXIT
}

# resize window to default 80x24
resize()
{
    printf '\e[8;24;80t'
}

# increase history file size
HISTFILESIZE=1500
# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# don't try to complete an empty command
shopt -s no_empty_cmd_completion

# colors for ls
eval "$(dircolors -b)"

# aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi


##### PROMPT #####
# update PS1 with current title and prompt color
update_ps1() { PS1="$title$prompt"; }

# reset PS1 to default
reset_ps1()
{
    reset_title
    reset_color
    update_ps1
}

# set and reset static title
set_title()
{
    title=
    printf "\e]0;$(whoami)@$(hostname): $1\a"
    update_ps1
}
reset_title()
{
    title="$default_title"
    update_ps1
}

# set, reset and clear color prompt
set_color()
{
    color="$1"
    update_ps1
}
reset_color()
{
    color="$default_color"
    update_ps1
}
clear_color()
{
    color=0m
    update_ps1
}

default_title='\[\e]0;\u@\h:\w\a\]'
title="$default_title"

default_color='0m'
color="$default_color"

# trim base directories in \w when pwd has more than N directories
PROMPT_DIRTRIM=7

base_prompt='\u@\h:\w'

# cutoff for a long pwd
long_pwd=40
# print a newline if pwd is long
newline_if_long='\[$(if [ ${#PWD} -gt $long_pwd ]; then printf "\]\n\["; fi)\]'

suffix="$newline_if_long\\$ "

prompt="\[\e[\$color\]$base_prompt$suffix\[\e[0m\]"

# initialize PS1
update_ps1
