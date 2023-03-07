# helper function for working with bare repo home directory
# see: for setup
# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

function git ()
{
    ARGS=("$@");

    if [ "$PWD" == "$HOME" ];
    then
       if [ "$1" == "status" ];
       then ARGS+=("-uno");
       fi;
       /usr/bin/git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME" "${ARGS[@]}";
    else /usr/bin/git "${ARGS[@]}"
    fi;
}


export GIT_EDITOR=/usr/local/bin/emacsclient

# preserve bash history https://unix.stackexchange.com/questions/1288/preserve-bash-history-in-multiple-terminal-windows
#Don't record duplicate commands to history
# Maximum number of history lines on disk
HISTFILESIZE=50000
# Ignore duplicate lines
HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file
#shopt -s histappend
#  and reread it
# Save and reload the history after each command finishes
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'n'}history -a; history -c; history -r"

#Git completion
# source ~/git-completion.bash

export PATH=/Applications/Racket\ v8.4/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=$PATH:./node_modules/.bin
export PATH=/usr/local/opt/icu4c/sbin:$PATH

# Put blender in path
export PATH="$PATH:/Applications/blender.app/Contents/MacOS"

# homebrew Apple m1 ?
# https://medium.com/geekculture/error-cannot-install-in-homebrew-on-arm-processor-in-intel-default-prefix-fd2e5f5fee88
export PATH="/opt/homebrew/bin:$PATH"

# fucking python
#alias python=/usr/local/bin/python3
export PATH=$PATH:/usr/local/Cellar/python@2/2.7.14_3/bin
export PATH=$PATH:/Users/kevzettler/Library/Python/2.7/bin


# Rust
export PATH=$PATH:/Users/kevzettler/.cargo/env
export PATH=$PATH:/Users/kevzettler/.cargo/bin

export EDITOR=/usr/local/bin/emacs

# prevents iterm from overriding set tab names?
# https://superuser.com/questions/343747/how-do-i-stop-automatic-changing-of-iterm-tab-titles


#Git aware bash prompt
#export GITAWAREPROMPT=~/.bash/git-aware-promp
#source "${GITAWAREPROMPT}/main.sh"
#export PS1="\u@\h \W \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtrst\]\$ "
#export SUDO_PS1="\[$bakred\]\u@\h\[$txtrst\] \w\$ "

export NVM_DIR="$HOME/.nvm"
#[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
#[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


setTabTitle() {
  echo -ne "\033]0;${PWD##*/}\007"
}
# Update the ITerm Tab name to match the current directory name
if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND="setTabTitle;$PROMPT_COMMAND"
fi



# Piece-by-Piece Explanation:
# the if condition makes sure we only screw with $PROMPT_COMMAND if we're in an iTerm environment
# iTerm happens to give each session a unique $ITERM_SESSION_ID we can use, $ITERM_PROFILE is an option too
# the $PROMPT_COMMAND environment variable is executed every time a command is run
# see: ss64.com/bash/syntax-prompt.html
# we want to update the iTerm tab title to reflect the current directory (not full path, which is too long)
# echo -ne "\033;foo\007" sets the current tab title to "foo"
# see: stackoverflow.com/questions/8823103/how-does-this-script-for-naming-iterm-tabs-work
# the two flags, -n = no trailing newline & -e = interpret backslashed characters, e.g. \033 is ESC, \007 is BEL
# see: ss64.com/bash/echo.html for echo documentation
# we set the title to ${PWD##*/} which is just the current dir, not full path
# see: stackoverflow.com/questions/1371261/get-current-directory-name-without-full-path-in-bash-script
# then we append the rest of $PROMPT_COMMAND so as not to remove what was already there
# voilà!


# GO Paths... *fart noise*
# installed through homebrew with
# https://ahmadawais.com/install-go-lang-on-macos-with-homebrew/
export GOPATH="${HOME}/.go"
export GOROOT="$(brew --prefix golang)/libexec"
export GO111MODULE=on
export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"


# The following is NVM auto switching from: https://github.com/nvm-sh/nvm#automatically-call-nvm-use
# 2019-07-19
find-up () {
    path=$(pwd)
    while [[ "$path" != "" && ! -e "$path/$1" ]]; do
        path=${path%/*}
    done
    echo "$path"
}

#
# Run 'nvm use' automatically every time there's
# a .nvmrc file in the directory. Also, revert to default
# version when entering a directory without .nvmrc
#
enter_directory() {
if [[ $PWD == $PREV_PWD ]]; then
    return
fi

PREV_PWD=$PWD
if [[ -f ".nvmrc" ]]; then
    nvm use
    NVM_DIRTY=true
elif [[ $NVM_DIRTY = true ]]; then
    nvm use default
    NVM_DIRTY=false
fi
}

export PROMPT_COMMAND=enter_directory;


. "$HOME/.cargo/env"
