#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# # Prompt

# Based on
# Pure
# by Sindre Sorhus
# https://github.com/sindresorhus/pure

# For my own and others sanity
# git:
# %b => current branch
# %a => current action (rebase/merge)
# %R => top level
# prompt:
# %F => color dict
# %f => reset color
# %~ => current path
# %* => time
# %n => username
# %m => shortname host
# %(?..) => prompt conditional - %(condition.true.false)

prompt_asp_preexec() {
    if [[ -n $prompt_asp_git_fetch_pattern ]]; then
        # detect when git is performing pull/fetch (including git aliases).
        local -H MATCH MBEGIN MEND match mbegin mend
        if [[ $2 =~ (git|hub)\ (.*\ )?($prompt_asp_git_fetch_pattern)(\ .*)?$ ]]; then
            # we must flush the async jobs to cancel our git fetch in order
            # to avoid conflicts with the user issued pull / fetch.
            async_flush_jobs 'prompt_asp'
        fi
    fi

    typeset -g prompt_asp_cmd_timestamp=$EPOCHSECONDS
}

prompt_asp_preprompt_render() {
  setopt localoptions noshwordsplit
  # set color for git branch/dirty status, change color if dirty checking has
  # been delayed.
  local git_color=green
  [[ -n ${prompt_asp_git_last_dirty_check_timestamp+x} ]] && git_color=red

  # initialize the preprompt array.
  local -a preprompt_parts
  local -a rprompt_parts

  # set the username and path.
  preprompt_parts+=('%F{red}%n%f%F{white}@%F{magenta}%m%f:%F{blue}%~%f')

  # add git branch and dirty status info.
  typeset -gA prompt_asp_vcs_info
  if [[ -n $prompt_asp_vcs_info[branch] ]]; then
      preprompt_parts+=("%F{white} %F{$git_color}"'${prompt_asp_vcs_info[branch]}${prompt_asp_git_dirty}%f')
  fi

  # git pull/push arrows.
  if [[ -n $prompt_asp_git_arrows ]]; then
    rprompt_parts+=('%F{cyan}${prompt_asp_git_arrows}%f')
  fi


  # if a virtualenv is activated, display it in grey
  if [[ -n $VIRTUAL_ENV ]]; then
      preprompt_parts+=('%F{cyan} ${VIRTUAL_ENV:t}%f')
  fi
  # execution time.
  [[ -n $prompt_asp_cmd_exec_time ]] && rprompt_parts+=('%F{yellow}${prompt_asp_cmd_exec_time}%f')
  # show exit status in right prompt
  rprompt_parts+=('%(?..%F{red}$? ↵%f)')

  # construct the new prompt with a clean preprompt.
  local -ah ps1
  ps1=(
    "["
    ${(j. .)preprompt_parts}  # join parts, space separated
    "]"
    $prompt_newline           # separate preprompt and prompt.
    '%(?.%F{white}.%F{red})${ASP_PROMPT_SYMBOL:-%%}%f '
  )

  prompt="${(j..)ps1}"
  RPROMPT='${prompt_asp_keymap}${prompt_asp_keymap:+ }'"${(j. .)rprompt_parts}"

  # expand the prompt for future comparision.
  local expanded_prompt
  expanded_prompt="${(s%%)prompt}"

  if [[ $1 != precmd ]] && [[ $prompt_asp_last_prompt != $expanded_prompt ]]; then
    # redraw the prompt.
    zle && zle .reset-prompt
  fi

}

prompt_asp_precmd() {
    # check exec time and store it in a variable
    prompt_asp_check_cmd_exec_time
    unset prompt_asp_cmd_timestamp

    # preform async git dirty check and fetch
    prompt_asp_async_tasks

    # print the preprompt
    prompt_asp_preprompt_render "precmd"
}

prompt_asp_check_cmd_exec_time() {
    integer elapsed
    (( elapsed = EPOCHSECONDS - ${prompt_asp_cmd_timestamp:-$EPOCHSECONDS} ))
    typeset -g prompt_asp_cmd_exec_time=
    (( elapsed > ${ASP_CMD_MAX_EXEC_TIME:-5} )) && {
        prompt_asp_human_time_to_var $elapsed "prompt_asp_cmd_exec_time"
    }
}

# turns seconds into human readable time
# 165392 => 1d 21h 56m 32s
# https://github.com/sindresorhus/pretty-time-zsh
prompt_asp_human_time_to_var() {
    local human total_seconds=$1 var=$2
    local days=$(( total_seconds / 60 / 60 / 24 ))
    local hours=$(( total_seconds / 60 / 60 % 24 ))
    local minutes=$(( total_seconds / 60 % 60 ))
    local seconds=$(( total_seconds % 60 ))
    (( days > 0 )) && human+="${days}d "
    (( hours > 0 )) && human+="${hours}h "
    (( minutes > 0 )) && human+="${minutes}m "
    human+="${seconds}s"

    # store human readable time in variable as specified by caller
    typeset -g "${var}"="${human}"
}

prompt_asp_async_git_aliases() {
    # collect all git aliases that do pull/fetch
    setopt localoptions noshwordsplit
    local dir=$1
    local -a gitalias pullalias

    # we enter repo to get local aliases as well.
    builtin cd -q $dir

    # list all aliases and split on newline.
    gitalias=(${(@f)"$(command git config --get-regexp "^alias\.")"})
    for line in $gitalias; do
        parts=(${(@)=line})           # split line on spaces
        aliasname=${parts[1]#alias.}  # grab the name (alias.[name])
        shift parts                   # remove aliasname

        # check alias for pull or fetch (must be exact match).
        if [[ $parts =~ ^(.*\ )?(pull|fetch)(\ .*)?$ ]]; then
            pullalias+=($aliasname)
        fi
    done

    print -- ${(j:|:)pullalias}  # join on pipe (for use in regex).
}

prompt_asp_async_vcs_info() {
    setopt localoptions noshwordsplit
    builtin cd -q $1 2>/dev/null

    # configure vcs_info inside async task, this frees up vcs_info
    # to be used or configured as the user pleases.
    zstyle ':vcs_info:*' enable git
    zstyle ':vcs_info:*' use-simple true
    # only export two msg variables from vcs_info
    zstyle ':vcs_info:*' max-exports 2
    # export branch (%b) and git toplevel (%R)
    zstyle ':vcs_info:git*' formats '%b' '%R'
    zstyle ':vcs_info:git*' actionformats '%b|%a' '%R'

    vcs_info

    local -A info
    info[top]=$vcs_info_msg_1_
    info[branch]=$vcs_info_msg_0_

    print -r - ${(@kvq)info}
}

# fastest possible way to check if repo is dirty
prompt_asp_async_git_dirty() {
    setopt localoptions noshwordsplit
    local untracked_dirty=$1 dir=$2
    # use cd -q to avoid side effects of changing directory, e.g. chpwd hooks
    builtin cd -q $dir

    if [[ $untracked_dirty = 0 ]]; then
        command git diff --no-ext-diff --quiet --exit-code
    else
        test -z "$(command git status --porcelain --ignore-submodules -unormal)"
    fi

    return $?
}

prompt_asp_async_git_fetch() {
    setopt localoptions noshwordsplit
    # use cd -q to avoid side effects of changing directory, e.g. chpwd hooks
    builtin cd -q $1

    # set GIT_TERMINAL_PROMPT=0 to disable auth prompting for git fetch (git 2.3+)
    export GIT_TERMINAL_PROMPT=0
    # set ssh BachMode to disable all interactive ssh password prompting
    export GIT_SSH_COMMAND=${GIT_SSH_COMMAND:-"ssh -o BatchMode=yes"}

    command git -c gc.auto=0 fetch &>/dev/null || return 99

    # check arrow status after a successful git fetch
    prompt_asp_async_git_arrows $1
}

prompt_asp_async_git_arrows() {
    setopt localoptions noshwordsplit
    builtin cd -q $1
    command git rev-list --left-right --count HEAD...@'{u}'
}

prompt_asp_async_tasks() {
    setopt localoptions noshwordsplit

    # initialize async worker
    ((!${prompt_asp_async_init:-0})) && {
        async_start_worker "prompt_asp" -u -n
        async_register_callback "prompt_asp" prompt_asp_async_callback
        typeset -g prompt_asp_async_init=1
    }

    typeset -gA prompt_asp_vcs_info
    local -H MATCH MBEGIN MEND
    if ! [[ $PWD = ${prompt_asp_vcs_info[pwd]}* ]]; then
        # stop any running async jobs
        async_flush_jobs "prompt_asp"

        # reset git preprompt variables, switching working tree
        unset prompt_asp_git_dirty
        unset prompt_asp_git_last_dirty_check_timestamp
        unset prompt_asp_git_arrows
        unset prompt_asp_git_fetch_pattern
        prompt_asp_vcs_info[branch]=
        prompt_asp_vcs_info[top]=
    fi
    unset MATCH MBEGIN MEND

    async_job "prompt_asp" prompt_asp_async_vcs_info $PWD

    # # only perform tasks inside git working tree
    [[ -n $prompt_asp_vcs_info[top] ]] || return

    prompt_asp_async_refresh
}

prompt_asp_async_refresh() {
  setopt localoptions noshwordsplit

  if [[ -z $prompt_asp_git_fetch_pattern ]]; then
    # we set the pattern here to avoid redoing the pattern check until the
    # working three has changed. pull and fetch are always valid patterns.
    typeset -g prompt_asp_git_fetch_pattern="pull|fetch"
    async_job "prompt_asp" prompt_asp_async_git_aliases $working_tree
  fi

  async_job "prompt_asp" prompt_asp_async_git_arrows $PWD

  # do not preform git fetch if it is disabled or working_tree == HOME
  if (( ${ASP_GIT_PULL:-0} )) && [[ $working_tree != $HOME ]]; then
    # tell worker to do a git fetch
    async_job "prompt_asp" prompt_asp_async_git_fetch $PWD
  fi

  # if dirty checking is sufficiently fast, tell worker to check it again, or wait for timeout
  integer time_since_last_dirty_check=$(( EPOCHSECONDS - ${prompt_asp_git_last_dirty_check_timestamp:-0} ))
  if (( time_since_last_dirty_check > ${ASP_GIT_DELAY_DIRTY_CHECK:-1800} )); then
    unset prompt_asp_git_last_dirty_check_timestamp
    # check check if there is anything to pull
    async_job "prompt_asp" prompt_asp_async_git_dirty ${ASP_GIT_UNTRACKED_DIRTY:-0} $PWD
  fi
}

prompt_asp_check_git_arrows() {
    setopt localoptions noshwordsplit
    local arrows left=${1:-0} right=${2:-0}

    (( right > 0 )) && arrows+=${ASP_GIT_DOWN_ARROW:-⇣}
    (( left > 0 )) && arrows+=${ASP_GIT_UP_ARROW:-⇡}

    [[ -n $arrows ]] || return
    typeset -g REPLY=$arrows
}

prompt_asp_async_callback() {
  setopt localoptions noshwordsplit
  local job=$1 code=$2 output=$3 exec_time=$4 next_pending=$6
  local do_render=0

  case $job in
    prompt_asp_async_vcs_info)
      local -A info
      typeset -gA prompt_asp_vcs_info

      # parse output (z) and unquote as array (Q@)
      info=("${(Q@)${(z)output}}")
      local -H MATCH MBEGIN MEND
      # check if git toplevel has changed
      if [[ $info[top] = $prompt_asp_vcs_info[top] ]]; then
        # if stored pwd is part of $PWD, $PWD is shorter and likelier
        # to be toplevel, so we update pwd
        if [[ $prompt_asp_vcs_info[pwd] = ${PWD}* ]]; then
          prompt_asp_vcs_info[pwd]=$PWD
        fi
      else
        # store $PWD to detect if we (maybe) left the git path
        prompt_asp_vcs_info[pwd]=$PWD
      fi
      unset MATCH MBEGIN MEND

      # update has a git toplevel set which means we just entered a new
      # git directory, run the async refresh tasks
      [[ -n $info[top] ]] && [[ -z $prompt_asp_vcs_info[top] ]] && prompt_asp_async_refresh

      # always update branch and toplevel
      prompt_asp_vcs_info[branch]=$info[branch]
      prompt_asp_vcs_info[top]=$info[top]
      do_render=1
      ;;
    prompt_asp_async_git_aliases)
      if [[ -n $output ]]; then
        # append custom git aliases to the predefined ones.
        prompt_asp_git_fetch_pattern+="|$output"
      fi
      ;;
    prompt_asp_async_git_dirty)
      local prev_dirty=$prompt_asp_git_dirty
      if (( code == 0 )); then
        unset prompt_asp_git_dirty
      else
        typeset -g prompt_asp_git_dirty="*"
      fi

      [[ $prev_dirty != $prompt_asp_git_dirty ]] && do_render=1

      # When prompt_asp_git_last_dirty_check_timestamp is set, the git info is displayed in a different color.
      # To distinguish between a "fresh" and a "cached" result, the preprompt is rendered before setting this
      # variable. Thus, only upon next rendering of the preprompt will the result appear in a different color.
      (( $exec_time > 5 )) && prompt_asp_git_last_dirty_check_timestamp=$EPOCHSECONDS
      ;;
    prompt_asp_async_git_fetch|prompt_asp_async_git_arrows)
      # prompt_asp_async_git_fetch executes prompt_asp_async_git_arrows
      # after a successful fetch.
      if (( code == 0 )); then
        local REPLY
        prompt_asp_check_git_arrows ${(ps:\t:)output}
        if [[ $prompt_asp_git_arrows != $REPLY ]]; then
          typeset -g prompt_asp_git_arrows=$REPLY
          do_render=1
        fi
      elif (( code != 99 )); then
        # Unless the exit code is 99, prompt_asp_async_git_arrows
        # failed with a non-zero exit status, meaning there is no
        # upstream configured.
        if [[ -n $prompt_asp_git_arrows ]]; then
          unset prompt_asp_git_arrows
          do_render=1
        fi
      fi
      ;;
  esac
  if (( next_pending )); then
    (( do_render )) && typeset -g prompt_asp_async_render_requested=1
    return
  fi

  [[ ${prompt_asp_async_render_requested:-$do_render} = 1 ]] && prompt_asp_preprompt_render
  unset prompt_asp_async_render_requested
}

prompt_asp_keymap_select(){
    typeset -g prompt_asp_keymap=
    [[ $KEYMAP == vicmd ]] && prompt_asp_keymap='%F{yellow}-- NORMAL --%f'
    zle reset-prompt
}


prompt_asp_setup() {
  # Prevent percentage showing up if output doesn't end with a newline.
  export PROMPT_EOL_MARK=''

  # disallow python virtualenvs from updating the prompt
  export VIRTUAL_ENV_DISABLE_PROMPT=1

  prompt_opts=(subst percent)

  # borrowed from promptinit, sets the prompt options in case asp was not
  # initialized via promptinit.
  setopt noprompt{bang,cr,percent,subst} "prompt${^prompt_opts[@]}"

  if [[ -z $prompt_newline ]]; then
    # This variable needs to be set, usually set by promptinit.
    typeset -g prompt_newline=$'\n%{\r%}'
  fi

  zmodload zsh/datetime
  zmodload zsh/zle
  zmodload zsh/parameter

  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info
  autoload -Uz async && async

  add-zsh-hook precmd prompt_asp_precmd
  add-zsh-hook preexec prompt_asp_preexec

  # prompt turns red if the previous command didn't exit with 0
  SPROMPT="Correct %F{red}%R%f to %F{green}%r%f [(y)es (n)o (a)bort (e)dit]? "

  zle -N zle-keymap-select prompt_asp_keymap_select
}

prompt_asp_setup "$@"
