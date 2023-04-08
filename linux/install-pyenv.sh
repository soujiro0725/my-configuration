#!/bin/bash

sudo apt-get update
sudo apt-get install git vim
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
echo 'export PYENV_ROOT=$HOME/.pyenv' >> ~/.bashrc
echo 'export PATH=$PYENV_ROOT/bin:$PATH' >> ~/.bashrc
echo 'eval "$(pyenv init -)"' >> ~/.bashrc
git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
echo 'eval "$(pyenv virtualenv-init -)"' >> ~/.bashrc
echo 'installing pyenv-update...'
git clone https://github.com/pyenv/pyenv-update.git ~/.pyenv/plugins/pyenv-update

