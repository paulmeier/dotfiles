export EDITOR="emacs -nw"

export VIMINIT="source ~/.vim/vimrc"

export PATH="$PATH:$HOME/.dotfiles/.bin"

export PATH="$HOME/.emacs.d/bin:$PATH"

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  alias pip=/usr/local/bin/pip3
fi

alias pmacs='emacs -q --load "~/.pmacs.d/init.el"'
