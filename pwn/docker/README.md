# docker image for pwn
----

```
git clone https://github.com/soujiro0725/my-configuration.git
cd my-configuration/pwn/docker/
./build.sh
```

```
まず、作業したフォルダに移動。そのフォルダに問題対象のバイナリやらを置く。
そこで
$ docker run --rm -v $PWD:/pwd --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -p 5555:5555 -i ubuntu18:ctf
実行したらこのターミナルは放置。

新しいターミナル、またはタブを開く。そこで
$ docker ps
CONTAINER ID    IMAGE           ...
1cc5488da0c2    ubuntu18:ctf    ...
↓
$ docker exec -it {CONTAINER ID} /bin/bash

そうするとコンテナに入れるはず。
cd /pwd/ すると、作業フォルダが見れるはず。

コードはMacホストで書いて、実行やCUIデバッガーはコンテナ内で実行する。 
```
