#!/usr/bin/env bash

_octez-client_complete() {
  local cur_word prev_word

  cur_word="${COMP_WORDS[COMP_CWORD]}"
  prev_word="${COMP_WORDS[COMP_CWORD - 1]}"

  # Tezos script
  script=${COMP_WORDS[0]}

  reply=$($script bash_autocomplete "$prev_word" "$cur_word" "${COMP_WORDS[@]}" 2> /dev/null)

  COMPREPLY=("$(compgen -W "$reply" -- "$cur_word")")

  return 0
}

_tezos-alphanet_complete() {
  script="${COMP_WORDS[0]}"
  second="${COMP_WORDS[1]}"
  cur_word="${COMP_WORDS[COMP_CWORD]}"
  case "$second" in
  container)
    COMPREPLY=("$(compgen -W "start stop status" -- "$cur_word")")
    ;;
  node)
    COMPREPLY=("$(compgen -W "start stop status log" -- "$cur_word")")
    ;;
  baker)
    COMPREPLY=("$(compgen -W "start stop status log" -- "$cur_word")")
    ;;
  client) ;;
    # prev_word="${COMP_WORDS[COMP_CWORD-1]}"
    # unset COMP_WORDS[0]
    # echo $script client bash_autocomplete "$prev_word" "$cur_word" ${COMP_WORDS[@]:1} > /tmp/completions
    # reply=$($script client bash_autocomplete "$prev_word" "$cur_word" ${COMP_WORDS[@]:1})
    # COMPREPLY=$($(compgen -W "$reply" -- $cur_word));;
  *)
    COMPREPLY=("$(compgen -W "start restart \
                             clear status stop kill head \
                             go_alpha_go shell client check_script update_script \
                             container node baker" -- "$cur_word")")
    ;;
  esac
  return 0
}

# Register _pss_complete to provide completion for the following commands
complete -F _octez-client_complete octez-client
complete -F _octez-client_complete octez-admin-client
complete -F _octez-client_complete octez-baker-alpha
complete -F _octez-client_complete octez-accuser-alpha
