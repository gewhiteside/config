# ~/.bash_aliases
# George Whiteside

# colors for ls and grep
# ignore backup files in ls and grep
alias ls='ls --color=auto --hide="*~" --hide="*#"'
alias la='ls -A --color=auto'
alias grep='grep --color=auto --exclude=*~ --exclude=*#'
alias fgrep='fgrep --color=auto --exclude=*~ --exclude=*#'
alias egrep='egrep --color=auto --exclude=*~ --exclude=*#'

# ignore case in less search and turn off bell
alias less='less -Qi'

# shut up gdb
alias gdb='gdb -q'
