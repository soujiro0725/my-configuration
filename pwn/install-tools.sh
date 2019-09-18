# install gef
wget -q -O- https://github.com/hugsy/gef/raw/master/gef.sh | sh

# install radare2
cd ~/Projects
git clone https://github.com/radare/radare2.git
cd radare2
./sys/user.sh

# install socat
sudo apt-get install socat

# install capstone
sudo apt-get install libcapstone3 libcapstone-dev
