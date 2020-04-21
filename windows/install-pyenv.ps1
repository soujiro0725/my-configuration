# install pyenv environment
if (!(get-command pyenv -errorAction SilentlyContinue)) {
  pip install pyenv-win --target $env:USERPROFILE\.pyenv
  [Environment]::SetEnvironmentVariable("PATH", "$env:Path;$env:USERPROFILE\.pyenv\pyenv-win\bin;$env:USERPROFILE\.pyenv\pyenv-win\shims", "User")
  $env:PATH = "$env:Path;$env:USERPROFILE\.pyenv\pyenv-win\bin;$env:USERPROFILE\.pyenv\pyenv-win\shims"
}
