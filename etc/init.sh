#!/bin/bash
#
# Automatically unpack dotfiles from github repository in your local $HOME directory

#set -o errexit
set -o nounset
set -o pipefail

##################################################################
# Helper functions
##################################################################
has() {

  which "$1" >/dev/null 2>&1


}

##################################################################
# Main script
# Globals(used and modified):
#   used: DOTFILES_PATH, GITHUB_REPO_URL
# Arguments:
#   None
# Returns(other then default exit status of the last command run):
#   None
##################################################################
main() {

  if has "git"; then
    # For test
    git clone -b test "$GITHUB_REPO_URL" || exit 0
    # git clone "$GITHUB_REPO_URL" || exit 0
  elif has "curl" || has "wget"; then
    if has "curl"; then
      :
    elif has "wge"; then
      :
    fi
  fi

}


DOTFILES_PATH="$HOME/.dotfiles"
GITHUB_REPO_URL='https://github.com/Himalayan-wildcat/dotfiles.git'

#cd "$(dirname "$0")" || exit 0
cd "$HOME" || exit 0
if [[ -d "$DOTFILES_PATH" ]]; then
  while :; do
    read -rp 'Dotfiles already exist. Do you want to overwrite the directory? [y/n]' input
    case "$input" in
      [yY]*) main
	     break
	     ;;
      [nN]*) echo 'Aborted!!'
	     exit 0
	     ;;
      *) echo "Aborted!! Invalid value. Enter [y/n]."
	 clear
	 continue
	 ;;
    esac
  done
else
  main
fi

exit 0
