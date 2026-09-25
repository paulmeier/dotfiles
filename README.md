# dotfiles

## Setting up a new machine

1. Install the prerequisites, plus the Proton Pass CLI (`pass-cli`) on `PATH`:

   ```sh
   brew install git stow starship
   ```

2. Install oh-my-zsh and its two plugins. The installer writes its own
   `~/.zshrc`; delete it so `stow` can link this repo's.

   ```sh
   sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
   git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
   git clone https://github.com/zsh-users/zsh-syntax-highlighting ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
   rm ~/.zshrc
   ```

3. Clone and link:

   ```sh
   git clone git@github.com:paulmeier/dotfiles.git ~/dotfiles
   cd ~/dotfiles && stow .
   ```

4. Load secrets from Proton Pass into `~/.zshrc.local`:

   ```sh
   pass-cli login
   exec zsh
   pp-sync
   ```

5. Install Doom Emacs (after `pp-sync`, so Doom's saved environment includes
   your email and GPG key):

   ```sh
   git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
   ~/.config/emacs/bin/doom install
   ```

After changing a secret later, run `pp-sync` and then `doom env`.
