# ~/.bash_aliases
# George Whiteside

# colors for ls and grep
# ignore backup files in ls and grep
alias ls='ls --color=auto --hide="*~" --hide="*#"'
alias la='ls -A --color=auto'
alias grep='grep --color=auto --exclude=*~ --exclude=*#'
alias fgrep='fgrep --color=auto --exclude=*~ --exclude=*#'
alias egrep='egrep --color=auto --exclude=*~ --exclude=*#'

# shut up gdb
alias gdb='gdb -q'

# add hyphenated aliases for functions I use interactively
alias ssh-start=ssh_start
alias open-notes=open_notes
