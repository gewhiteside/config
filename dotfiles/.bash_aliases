# ~/.bash_aliases
# George Whiteside

# Use colors when appropriate and ignore backup files in ls and grep.
alias ls='ls --color=auto --hide="*~" --hide="*#"'
alias la='ls -A --color=auto'

grep_opts='--color=auto --exclude=*~ --exclude=*#'
_set_grep_opts()
{
    for grep in grep egrep fgrep; do
        alias $grep="$grep $grep_opts"
    done

    # Not all distros have an rgpep.
    alias rgrep="grep -r $grep_opts"
}
_set_grep_opts

# Shut up gdb.
alias gdb='gdb -q'

# Add hyphenated aliases for functions I use interactively.
alias ssh-start=ssh_start

# Alias common screen commands.
alias screenmacs='screen -dR emacs emacs'
alias screenshell='screen -dR shell'
