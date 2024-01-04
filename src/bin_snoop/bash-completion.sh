_octez-snoop_complete() {
  local cur_word prev_word prefix

  cur_word="${COMP_WORDS[COMP_CWORD]}"
  prev_word="${COMP_WORDS[COMP_CWORD - 1]}"

  # Tezos script
  script=${COMP_WORDS[0]}

  test_empty=$($script bash_autocomplete "$prev_word" "" ${COMP_WORDS[@]} 2> /dev/null)

  if [ -z "$test_empty" ]; then
    COMPREPLY=($(compgen -f -- $cur_word))
  else

    reply_0=$($script bash_autocomplete "$prev_word" "$cur_word" ${COMP_WORDS[@]} 2> /dev/null)

    reply=$(echo "$reply_0" | sed -r "s:^$cur_word([^/]*/?).*$:$cur_word\1:g")

    COMPREPLY=($(compgen -W "$reply" -- $cur_word))
    COMPREPLY=($(echo "${COMPREPLY[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))

    while [[ ${#COMPREPLY[@]} == 1 && ${COMPREPLY[0]:0-1} == "/" ]]; do
      reply=$(echo "$reply_0" | sed -r "s:^${COMPREPLY[0]}([^/]*/?).*$:${COMPREPLY[0]}\1:g")

      COMPREPLY=($(compgen -W "$reply" -- $cur_word))
      COMPREPLY=($(echo "${COMPREPLY[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))

    done

  fi

  return 0
}

# Register _pss_complete to provide completion for the following commands
complete -F _octez-snoop_complete octez-snoop
