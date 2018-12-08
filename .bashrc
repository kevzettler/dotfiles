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

#Don't record duplicate commands to history
export HISTCONTROL=ignoreboth:erasedups


# RBENV init
eval "$(rbenv init -)";

#Git completion
source ~/git-completion.bash


export GOPATH=/Users/kev/code/go
export PATH=/Users/kev/code/go/bin:$PATH

export PATH=/usr/local/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH
export PATH=$PATH:/usr/local/Cellar/python@2/2.7.14_3/bin
export PATH=$PATH:/Users/kevzettler/Library/Python/2.7/bin
export PATH=$PATH:./node_modules/.bin
export PATH=/usr/local/opt/icu4c/sbin:$PATH

export EDITOR=/usr/local/bin/emacs


# prevents iterm from overriding set tab names?
# https://superuser.com/questions/343747/how-do-i-stop-automatic-changing-of-iterm-tab-titles
export TERM=vt100

#Git aware bash prompt
#export GITAWAREPROMPT=~/.bash/git-aware-prompt
#source "${GITAWAREPROMPT}/main.sh"
#export PS1="\u@\h \W \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtrst\]\$ "
#export SUDO_PS1="\[$bakred\]\u@\h\[$txtrst\] \w\$ "


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# put this in your .bash_profile
if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND='echo -ne "\033];${PWD##*/}\007"; ':"$PROMPT_COMMAND";
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
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /Users/kev/.config/yarn/global/node_modules/tabtab/.completions/serverless.bash ] && . /Users/kev/.config/yarn/global/node_modules/tabtab/.completions/serverless.bash
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /Users/kev/.config/yarn/global/node_modules/tabtab/.completions/sls.bash ] && . /Users/kev/.config/yarn/global/node_modules/tabtab/.completions/sls.bash
