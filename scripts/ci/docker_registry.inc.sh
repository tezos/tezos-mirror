#!/usr/bin/env bash

# A Docker tag name must be valid ASCII and may contain lowercase and
# uppercase letters, digits, underscores, periods and dashes. A tag
# name may not start with a period or a dash and may contain a maximum
# of 128 characters.
sanitizeTag() {
  tr -c -- '-._\n[:alnum:]' '_'
}
