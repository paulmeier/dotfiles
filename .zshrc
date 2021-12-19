export EDITOR="emacs -nw"

export VIMINIT="source ~/.vim/vimrc"

export PATH="$PATH:$HOME/.dotfiles/.bin"

export PATH="$HOME/.emacs.d/bin:$PATH"

#export GOPATH=$HOME/go
#export GOBIN=$HOME/go/bin
#export PATH="$PATH:$GOBIN"
#export GO111MODULE=on

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
[ -f "$HOME/.fzf.zsh" ] && source "$HOME/.fzf.zsh"
#export FZF_DEFAULT_OPS="--extended"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

bindkey -v

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

alias pip=/usr/local/bin/pip3

alias pmacs='emacs -q --load "~/.pmacs.d/init.el"'
