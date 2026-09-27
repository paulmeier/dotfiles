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

## Emacs profiles

Doom is the default `emacs`. [Emacs Writing Studio](https://github.com/pprevos/emacs-writing-studio)
runs as a separate profile in `~/.config/ews` via `--init-directory`, so the
two never share packages or state:

| Profile | Launch                              | Keys                   |
|---------|-------------------------------------|------------------------|
| Doom    | `emacs`                             | evil, `SPC` leader     |
| EWS     | `ews` (GUI) / `ews-nw` (terminal)   | stock Emacs, `C-c w`   |

Only my files are tracked: `early-init.el` (hooks, dictionary, GitHub ELPA mirror since ProtonVPN gets blocked by elpa.gnu.org)
and `user.el` (paths, theme, font, extras). Data paths come from Doom's
gitignored `local.el`. After `stow .`, fetch upstream once (pass a git ref to
upgrade):

```sh
~/.config/ews/bootstrap.sh
```

Packages install into `~/.config/ews/elpa` on first launch. macOS needs
`hunspell` with `en_US` in `~/Library/Spelling`, and `coreutils` for `gls`.
