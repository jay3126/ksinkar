#personal aliases
alias l.='ls -d .* --color=auto'
alias emacs='emacs -nw'

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.


# If this is an xterm set the title to user@host:dir

function get_workspace {
  if [[ "$PWD" =~ workspace ]]; then
      SED_SCRIPT='s_'$HOME'/workspace/__'
      WORKSPACE=`echo $PWD | sed $SED_SCRIPT`
  else
      WORKSPACE=$PWD
  fi
  echo $WORKSPACE
}


case "$TERM" in
xterm*|rxvt*)
     trap 'echo -ne "\e]0;`get_workspace`: $BASH_COMMAND\007"' DEBUG
    ;;
*)
    ;;
esac


#PS1='[`__git_ps1` \W]\$'

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}


export MYSQL_PS1="\u@\h [\d]> "

# prompt from http://webcache.googleusercontent.com/search?q=cache:http://opinionated-programmer.com/2011/01/colorful-bash-prompt-reflecting-git-status/

function _git_prompt() {
    local git_status="`git status -unormal 2>&1`"
    if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
        if [[ "$git_status" =~ Untracked\ files ]]; then
            local ansi=36
        elif [[ "$git_status" =~ nothing\ to\ commit ]]; then
            local ansi=37
        elif [[ "$git_status" =~ Changes\ to\ be\ committed ]]; then
            local ansi=33
        else
            local ansi=31
        fi

        if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
            branch=${BASH_REMATCH[1]}
            #test "$branch" != master || branch=' '
        else
            # Detached HEAD.  (branch=HEAD is a faster alternative.)
            branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null ||
                echo HEAD`)"
        fi
        echo -n '\[\e[0;'"$ansi"'m\]'"($branch)"'\[\e[0m\] '
    fi
}
function i() {
    PS1="`_git_prompt`"'\[\e[1;34m\]\W ॐ \[\e[0m\] '

}
PROMPT_COMMAND=i
