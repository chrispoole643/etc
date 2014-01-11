Dotfiles
========

Collection of configuration files for emacs, tmux, and zsh.

To deploy:

    stow -t$HOME emacs tmux zsh

When you run Emacs for the first time, it'll fetch packages from [melpa][m], so may
take a while. To compile all the emacs lisp files, run

    cjp-recompile-emacs-setup



[m]: http://melpa.milkbox.net
