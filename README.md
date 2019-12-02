# Dotfiles

A collection of configuration files for emacs, tmux, zsh, etc.

To deploy, I store these files in an `etc` directory, and use [stow][]
to link them into my home directory:

```bash
stow -t$HOME emacs tmux zsh git
```

I use [Straight.el][straight] for Emacs package management. Thus, when
you run Emacs for the first time with this confirguration, it'll fetch
packages from [melpa][], so may take a while to load.


[stow]: https://www.gnu.org/software/stow/
[straight]: https://github.com/raxod502/straight.el
[melpa]: https://melpa.org/
