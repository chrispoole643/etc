################################################################################
### zsh configuration
### =================
###
### Detect platform, and load settings
################################################################################


if [[ ${OSTYPE%%.*} == "darwin13" ]]; then
    platform="mac"
    source "$HOME/.zshrc-mac"
elif [[ "$OSTYPE" == *linux* ]]; then
    platform="linux"
    source "$HOME/.zshrc-linux"
fi

# Start tmux
st
