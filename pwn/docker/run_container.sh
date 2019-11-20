cd ~/Projects/ctf-memo/
docker run --rm --privileged -v $PWD:/pwd --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -p 5555:5555 -i ubuntu18:ctf
