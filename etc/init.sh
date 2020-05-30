#!/bin/bash
#
# Automatically unpack dotfiles from github repository in your local $HOME directory

set -o errexit
#set -o nounset
set -o pipefail

##################################################################
# Helper functions
##################################################################
logging() {

  :

}

logger_info() {

  :

}

logger_error() {

  :

}

has() {

  which "$1" >/dev/null 2>&1


}


##################################################################
# Create symbolic links of the cloned dotfiles
# Globals(used and modified):
#   used: DOTFILES_PATH, GITHUB_REPO_URL
# Arguments:
#   None
# Returns(other then default exit status of the last command run):
#   None
##################################################################
initialize_dotfile() {

  for dot_file in "$DOTFILES_PATH"/.??*; do
    [[ "$dot_file" == "${DOTFILES_PATH}/.git" ]] && continue
    ln -sfnv "$dot_file" "$HOME" || exit
  done

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
    git clone -b test "$GITHUB_REPO_URL" || exit
    # git clone "$GITHUB_REPO_URL" || exit
    initialize_dotfile
  elif has "curl" || has "wget"; then
    if has "curl"; then
      :
    elif has "wge"; then
      :
    fi
  fi

}

readonly REPO_NAME='dotfiles'
readonly DOTFILES_PATH="$HOME/${REPO_NAME}"
readonly GITHUB_REPO_URL="https://github.com/Himalayan-wildcat/${REPO_NAME}.git"

# TODO:
## add if condition based on OS
## add if condition based on shell(bash)

cd "$HOME" || exit
if [[ -d "$DOTFILES_PATH" ]]; then
  # Terminate the script if the dotfiles dir already exists.
  # The dir should NOT be overwritten by executing 'rm -rf $HOME/dotfiles', which can
  # inadvertently execute 'rm -rf $HOME'(it happnes really ... :<)

  # TODO:
  # To replace echo with logging function
  echo 'Dotfiles directory already exist. Retry again after deleging it.'
  exit 0
else
  main
fi

exit 0
