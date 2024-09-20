#!/usr/bin/env bash

##
# Copy an Anki card's template to the clipboard.
#
# All the templates are in ./anki/card-templates/. Copy a template
# with a command line like this one:
#
#   $ ./anki-templ.sh front
##

err_no_templ_param=1
err_no_templ_found=2

if [[ $# == 0 ]]
then
  printf '\nProvide the name of the template as the first parameter.\n'
  exit $err_no_templ_param
fi

templ="./anki/templates/${1}.html"

if [[ ! -r $templ ]]
then
  printf '\nTemplate ‘%s’ not found.\n' "$templ"
  exit $err_no_templ_found
else
  ##
  # Copy the contents of the template into the clipboard.
  #
  0< "$templ" xclip -in -selection clipboard
fi
