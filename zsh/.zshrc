################################################################################
### zsh configuration
### =================
###
### Detect platform, and load settings
################################################################################


source "$HOME/.zshrc-common-head"

if [[ ${OSTYPE%%.*} == "darwin13" ]] || [[ ${OSTYPE%%.*} == "darwin14" ]]; then
    platform="mac"
    source "$HOME/.zshrc-mac"
elif [[ "$OSTYPE" == *linux* ]]; then
    platform="linux"
    source "$HOME/.zshrc-linux"
fi

source "$HOME/.zshrc-common-tail"

# Start tmux (with my configuration) if it isn't already running
tmux list-sessions 2>&1 > /dev/null
if [[ $? -eq 1 ]]; then
    st
fi
