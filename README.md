Dotfiles
========

Collection of configuration files for emacs, tmux, and zsh.

To deploy, I store these files in an `etc` directory, and use [stow][s] to link
them into my home directory:

    stow -t$HOME emacs tmux zsh

When you run Emacs for the first time, it'll fetch packages from [melpa][m]
(after asking you for confirmation), so may take a while.

[m]: http://melpa.milkbox.net
[s]: http://www.gnu.org/software/stow
