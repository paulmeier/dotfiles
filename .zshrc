SYSTEM="$(uname -s)"

HOST="$(hostname)"

export VIMINIT="source ~/.vim/vimrc"
export PATH="$HOME/.emacs.d/bin:$PATH"
export EDITOR="emacs -nw"
export ZSH=$HOME/.oh-my-zsh
export GOPATH=$HOME/go
export GOBIN=$HOME/go/bin
export GO111MODULE=on

export PATH="$PATH:$HOME/.dotfiles/.bin"

path+=($GOBIN)
export PATH

ZSH_THEME="spaceship"

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias pmacs='emacs -q --load "~/.pmacs.d/init.el"'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

export PATH="/usr/local/opt/icu4c/bin:$PATH"

export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
[ -f "$HOME/.fzf.zsh" ] && source "$HOME/.fzf.zsh"
#export FZF_DEFAULT_OPS="--extended"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

bindkey -v

if [ "$(hostname)" = "S292266.local" ]; then
    export GEN_ROCKET_HOME=~/genrocket
    export PATH=$PATH:$GEN_ROCKET_HOME/bin
    export JAVA_OPTS="-Xms512m -Xmx2048m --add-opens java.base/jdk.internal.loader=ALL-UNNAMED --add-opens jdk.zipfs/jdk.nio.zipfs=ALL-UNNAMED"
fi

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

alias pip=/usr/local/bin/pip3
