################################################################################
#### zsh configuration
#### =================
####
#### Apply common settings, load platform-specific file
################################################################################


################################################################################
### Environment
################################################################################


export PAGER=less
export EDITOR='emacsclient'

HISTFILE="$HOME/.zhistory"
HISTSIZE=3000
SAVEHIST=3000


################################################################################
### zsh setup
################################################################################


zstyle :compinstall filename '$HOME/.zshrc'
# Tab completion, etc.
autoload -Uz compinit promptinit
compinit -u
promptinit

setopt appendhistory     # append to HISTFILE
setopt autocd            # go to directories without "cd"
setopt extendedglob      # wacky zsh-specific pattern matching
setopt nocheckjobs       # don't complain about background jobs on exit
setopt nohup             # don't kill background jobs on exit
#setopt printexitvalue   # print exit value from jobs
setopt noautoremoveslash

setopt correct           # command correction
setopt extendedhistory	 # puts timestamps in the history
setopt histallowclobber
setopt histreduceblanks
setopt multios           # now we can pipe to multiple outputs!
setopt incappendhistory 
setopt sharehistory
setopt nobeep            # beeps are annoying
setopt rmstarwait        # 10 second wait if you do something that will delete everything
setopt globcomplete      # if we have a glob this will expand it
setopt menucomplete
setopt histignorealldups # even if there are commands inbetween commands that
                         # are the same, still only save the last one
setopt notify
setopt globdots
setopt pushdtohome
setopt cdablevars
setopt autolist
# setopt correctall
setopt recexact
setopt longlistjobs
setopt autoresume
setopt pushdsilent
setopt noclobber
setopt autopushd
setopt pushdminus
setopt rcquotes
setopt mailwarning

unsetopt autoparamslash
unsetopt allexport
unsetopt bgnice          # do not nice bg commands (don't set background commands at
                         # lower priority)

autoload -U select-word-style
select-word-style bash   # make word boundaries and stuff work like Bash

## Key binding

bindkey -e                 # Emacs-style keybindings
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

## Completion settings

# cache completions (useful for apt/dpkg package completions)
zstyle ':completion:*' use-cache onzstyle ':completion:*' cache-path $HOME/.zsh/cache

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
#zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31" # code completion
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:processes' command 'ps -axw'
zstyle ':completion:*:processes-names' command 'ps -awxho command'
# Completion Styles
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
#
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}') 
# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Separate man page sections.  Neat.
zstyle ':completion:*:manuals' separate-sections true

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
zstyle '*' hosts $hosts

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

# Don't prompt for a huge list, page it!
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Don't prompt for a huge list, menu it!
zstyle ':completion:*:default' menu 'select=0'

# generate descriptions with magic.
zstyle ':completion:*' auto-description 'specify: %d'

## Custom completion commands

# # pip zsh completion start
# function _pip_completion {
#     local words cword
#     read -Ac words
#     read -cn cword
#     reply=( $( COMP_WORDS="$words[*]" \
#         COMP_CWORD=$(( cword-1 )) \
#         PIP_AUTO_COMPLETE=1 $words[1] ) )
# }
# compctl -K _pip_completion pip
# # pip zsh completion end

## Add-ons

ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR="$HOME/.zsh-highlighters"

source "$HOME/.zsh-syntax-highlighting.zsh"
source "$HOME/.zsh-history-substring-search.zsh"

ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow'


################################################################################
### Platform-dependent functions and aliases
################################################################################


if [ ${OSTYPE%%.*} = "darwin10" ]; then
    if [ -f "$HOME/.macmini" ]; then
        platform="macmini"
    fi
    source "$HOME/.zshrc-mac"
elif [ "$OSTYPE" = "linux-gnu" ]; then
    platform="linux"
    source "$HOME/.zshrc-linux"
fi

## Load functions common to mac and linux
if [ -f "$HOME/.zshrc-common" ]; then
    . "$HOME/.zshrc-common"
fi

if [[ $platform == "macmini" ]]; then
    starttmux
fi
