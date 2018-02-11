# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

alias chrome='open -a /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'
alias preview='open -a /Applications/Preview.app/Contents/MacOS/Preview'

#for rvm :Ruby Version Manager
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
rvm use 2.4.2

# Customize to your needs...
export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# sdkman raises an error "file exists..."
set +o noclobber

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/soichi/.sdkman"
[[ -s "/Users/soichi/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/soichi/.sdkman/bin/sdkman-init.sh"
