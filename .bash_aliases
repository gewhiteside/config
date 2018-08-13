alias ssh-start='eval $(ssh-agent);ssh-add;trap "kill $SSH_AGENT_PID" EXIT'
alias less='less -Qi'
