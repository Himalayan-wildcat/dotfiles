# built-in sh
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias rm='rm -i'


# open chromium
alias cb='chromium-browser --full-screen >/dev/null 2>&1'

# git
alias ga='git add'
alias gcmsg='git commit -m'
alias gb='git branch'
alias gco='git checkout'
alias gst='git status'
alias gd='git diff'
alias gs='git stash'
alias gd='git diff'
alias defaultbranch="git symbolic-ref refs/remotes/origin/HEAD | awk -F '[/]' '{print $NF}'"
alias gt="git tag"
alias glogp="git log --pretty='format:%C(yellow)%h %C(green)%cd %C(reset)%s %C(red)%d %C(cyan)[%an]' --date=iso"
# alias gparent='git show-branch | grep * | grep -V $(git rev-parse --abrev-ref HEAD) | head -1 | awk -F '[]~^[]' '{print $2}''

# clipboard
alias c='xsel --clipboard --input'
alias p='xsel --clipboard --output'

#alias python='python3'

# emacs
alias e='emacs -nw'

# tmux
alias t='tmux'

# kubectl
# for EKS cluster
alias k-eks-test='kubectl --kubeconfig=/home/hiroaki/.kube/tmp/1005/test-config'
alias k-qa='kubectl --kubeconfig=$HOME/.kube/minarai/eks/qa/config'
alias k-stg='kubectl --kubeconfig=$HOME/.kube/minarai/eks/stg/config'
alias k-prod='kubectl --kubeconfig=$HOME/.kube/minarai/eks/prod/config'

# for AKS cluster
alias k-anoi-prod='kubectl --kubeconfig=$HOME/.kube/anoi/prod/config'
alias k-anoi-stg='kubectl --kubeconfig=$HOME/.kube/anoi/stg/config'
alias k-gs-prod='kubectl --kubeconfig=$HOME/.kube/gs/prod/config'

# stern
alias stern-qa='stern --kubeconfig=$HOME/.kube/minarai/eks/qa/config'
alias stern-stg='stern --kubeconfig=$HOME/.kube/minarai/eks/stg/config'
alias stern-prod='stern --kubeconfig=$HOME/.kube/minarai/eks/prod/config'


# watch
alias watch='watch '
alias tf='terraform'
