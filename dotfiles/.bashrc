# ~/.bashrc
# George Whiteside

# If shell is not interactive, don't do anything.
[[ $- != *i* ]] && return

##### GENERAL #####
# Increase the size of the  history file.
HISTFILESIZE=3000
# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it.
shopt -s histappend

# Don't try to complete an empty command.
shopt -s no_empty_cmd_completion

# Add colors for ls.
eval "$(dircolors -b)"

# Start ssh-agent and kill it on exit.
ssh_start()
{
    eval $(ssh-agent)
    ssh-add
    trap "kill $SSH_AGENT_PID" EXIT
}


##### MACHINE-SPECIFIC ENVIRONMNET #####
# The following variables are intialized to a default value below. They can be
# changed in the machine-specific environment file "~/.$(hostname -s)_bash" to
# override the default behavior.

# Color of the environment indicator in the prompt.
env_color='0;36m'
# Color of user and host names in the prompt.
user_host_color='1;32m'
# Color of symbols in the prompt (i.e. : and \$).
symbol_color='0m'
# Color of the pwd in the prompt.
pwd_color='1;34m'
# Color of the git information in the prompt.
git_color='0;33m'


#### OTHER FILES ####
source ~/.bash_aliases

# Source machine-specific environment.
# source ~/path-to-file/file

## GIT
source ~/.git-completion.bash
source ~/.git-prompt.sh


##### PROMPT #####
## TITLE ##
title='\[\e]0;\u@\h:\w\a\]'
prompt="$title"


## ENVIRONMENT INDICATOR ##
env=

# This value is initialized above without wrapping escape characters.
env_color="\[\e[$env_color\]"

# Set and clear the environment indicator.
set_env() { env="($1) "; }
clear_env() { env=; }

prompt="$prompt$env_color\$env"


## USER AND HOST ##
user_host='\u@\h'

# These values are initialized above without wrapping escape characters.
user_host_color="\[\e[$user_host_color\]"
symbol_color="\[\e[$symbol_color\]"

prompt="$prompt$user_host_color$user_host$symbol_color:"


## PWD ##
# TODO: Remove when I have implemented this for myself.
# Trim base directories in \w when pwd has more than N directories.
PROMPT_DIRTRIM=7

# TODO: Trim base directories when pwd is large.
# TODO: Document this function.
# Should be called every time the user is prompted.
pwd_prompt()
{
    local tmp=$PWD
    # Replace $HOME with tilde.
    tmp="${tmp/$HOME/\~}"
    # Replace $pr with ellipses.
    [ $pr ] && tmp="${tmp/${pr#~/}\//.../}"
    echo "$tmp"
}

# This value is initialized above without wrapping escape characters.
pwd_color="\[\e[$pwd_color\]"

prompt="$prompt$pwd_color\$(pwd_prompt)"


## GIT ##
# See ~/.git-prompt.sh for more information.
# Set what information to show.
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWCOLORHINTS=1

# "Cache" the git info before each the prompt so that it can both be printed in
# the prompt and be checked in prompt_len without havind to call __git_ps1
# twice as doing so takes a significant amount of time with larger repos.
set_git_info() { git_info=$(__git_ps1); }
PROMPT_COMMAND=set_git_info

# This value is initialized above without wrapping escape characters.
git_color="\[\e[$git_color\]"

prompt="$prompt$git_color\$git_info"


## SUFFIX ##
# Cutoff for a long prompt.
long_prompt=50

# Calculates the length of the prompt (without a potential newline).
prompt_len()
{
    local pwd_prompt_var="$(pwd_prompt)"
    local hostname_short="${HOSTNAME%%.*}"
    echo $((${#env} + ${#USER} + ${#hostname_short} + ${#pwd_prompt_var} \
                    + ${#git_info} + 4))
}

# Print a newline if the prompt is too long. This can't be a function, as the
# newline is only printed if this is "inlined".
newline_if_long='\[$([ $(prompt_len) -gt $long_prompt ] && printf "\]\n\[")\]'

# Use the same color as the user and host names for the trailing symbol.
suffix="$newline_if_long$symbol_color\\$ "

no_color="\[\e[0m\]"

prompt="$prompt$suffix$no_color"


## PROMPT INIT ##
PS1="$prompt"
