# installation scoop
try {
 scoop --version
} finally {
  Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://get.scoop.sh')
  Set-ExecutionPolicy RemoteSigned -scope CurrentUser
}

# install some utility applications
scoop install python wget curl git
python --version
pip install pipenv

# install pyenv environment
if (!(get-command pyenv -errorAction SilentlyContinue)) {
  pip install pyenv-win --target $env:USERPROFILE\.pyenv
  [Environment]::SetEnvironmentVariable("PATH", "$env:Path;$env:USERPROFILE\.pyenv\pyenv-win\bin;$env:USERPROFILE\.pyenv\pyenv-win\shims", "User")
  $env:PATH = "$env:Path;$env:USERPROFILE\.pyenv\pyenv-win\bin;$env:USERPROFILE\.pyenv\pyenv-win\shims"
}

pyenv install 3.6.8-amd64

# create temporary file
if (!(Test-Path $env:USERPROFILE\.cache)) {
  New-Item $env:USERPROFILE\.cache -ItemType Directory
}
if (!(Test-Path $env:USERPROFILE\.cache\tmp)) {
  New-Item $env:USERPROFILE\.cache\tmp -ItemType Directory
}

# create project file
if (!(Test-Path $env:USERPROFILE\tensorflow_tutorial)) {
  New-Item $env:USERPROFILE\tensorflow_tutorial -ItemType Directory
}

# setup python environment
Set-Location $env:USERPROFILE\tensorflow_tutorial
pipenv install --python 3.6.8-amd64
pipenv install tensorflow==1.12.0 
pipenv install --dev python-language-server[all] ipython
