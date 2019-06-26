# ~/.bash_aliases
# George Whiteside

# Use colors when appropriate and ignore backup files in ls and grep.
alias ls='ls --color=auto --hide="*~" --hide="*#"'
alias la='ls -A --color=auto'

# Not all distros have an rgrep.
type rgrep &> /dev/null || alias rgrep='grep -r'

# Shut up gdb.
alias gdb='gdb -q'

# Alias common screen commands.
alias screenmacs='screen -dR emacs emacs'
alias screenshell='screen -dR shell'
