# ~/.bash_aliases
# George Whiteside

# Colors for ls and grep. Also, ignore backup files in ls and grep.
alias ls='ls --color=auto --hide="*~" --hide="*#"'
alias la='ls -A --color=auto'
alias grep='grep --color=auto --exclude=*~ --exclude=*#'
alias fgrep='fgrep --color=auto --exclude=*~ --exclude=*#'
alias egrep='egrep --color=auto --exclude=*~ --exclude=*#'

# Shut up gdb.
alias gdb='gdb -q'

# Add hyphenated aliases for functions I use interactively.
alias ssh-start=ssh_start

# Alias common screen commands.
alias screenmacs='screen -dR emacs emacs'
alias screenshell='screen -dR shell'
