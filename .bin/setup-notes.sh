#!/usr/bin/env bash

# Requirements
# stow
# watchman
# zsh
# emacs

# First dotfiles setup
cd ~
git clone git@github.com:paulmeier/dotfiles.git .dotfiles
cd ~/.dotfiles
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "Zsh.org")'
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "Doom.org")'
stow .

cd ~
git clone git@github.com:paulmeier/Org.git
git clone git@github.com:paulmeier/Org-Roam.git

cd ~/Org
watchman watch .
git config --bool branch.main.sync true
git config --bool branch.main.syncnewFiles true
watchman -- trigger . sync-trigger ".org" -- sync-notes.sh

cd ~/Org-Roam
watchman watch .
git config --bool branch.main.sync true
git config --bool branch.main.syncnewFiles true
watchman -- trigger . sync-trigger ".org" -- sync-notes.sh
