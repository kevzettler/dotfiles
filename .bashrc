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
# Maximum number of history lines in memory
export HISTSIZE=50000
# Maximum number of history lines on disk
export HISTFILESIZE=50000
# Ignore duplicate lines
export HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file
#  instead of overwriting it
shopt -s histappend

#  and reread it
# Save and reload the history after each command finishes
# export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"



#Git completion
source ~/git-completion.bash


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
#export GITAWAREPROMPT=~/.bash/git-aware-promp
#source "${GITAWAREPROMPT}/main.sh"
#export PS1="\u@\h \W \[$txtcyn\]\$git_branch\[$txtred\]\$git_dirty\[$txtrst\]\$ "
#export SUDO_PS1="\[$bakred\]\u@\h\[$txtrst\] \w\$ "

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

# put this in your .bash_profile
if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND='echo -ne "\033];${PWD##*/}\007"; ':"$PROMPT_COMMAND";
fi


# GO Paths... *fart noise*
# installed through homebrew with
# https://ahmadawais.com/install-go-lang-on-macos-with-homebrew/
export GOPATH="${HOME}/.go"
export GOROOT="$(brew --prefix golang)/libexec"
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

cdnvm(){
    cd "$@";
    nvm_path=$(find-up .nvmrc | tr -d '[:space:]')

    # If there are no .nvmrc file, use the default nvm version
    if [[ ! $nvm_path = *[^[:space:]]* ]]; then

        declare default_version;
        default_version=$(nvm version default);

        # If there is no default version, set it to `node`
        # This will use the latest version on your machine
        if [[ $default_version == "N/A" ]]; then
            nvm alias default node;
            default_version=$(nvm version default);
        fi

        # If the current version is not the default version, set it to use the default version
        if [[ $(nvm current) != "$default_version" ]]; then
            nvm use default;
        fi

        elif [[ -s $nvm_path/.nvmrc && -r $nvm_path/.nvmrc ]]; then
        declare nvm_version
        nvm_version=$(<"$nvm_path"/.nvmrc)

        declare locally_resolved_nvm_version
        # `nvm ls` will check all locally-available versions
        # If there are multiple matching versions, take the latest one
        # Remove the `->` and `*` characters and spaces
        # `locally_resolved_nvm_version` will be `N/A` if no local versions are found
        locally_resolved_nvm_version=$(nvm ls --no-colors $(<"./.nvmrc") | tail -1 | tr -d '\->*' | tr -d '[:space:]')

        # If it is not already installed, install it
        # `nvm install` will implicitly use the newly-installed version
        if [[ "$locally_resolved_nvm_version" == "N/A" ]]; then
            nvm install "$nvm_version";
        elif [[ $(nvm current) != "$locally_resolved_nvm_version" ]]; then
            nvm use "$nvm_version";
        fi
    fi
}
alias cd='cdnvm'
