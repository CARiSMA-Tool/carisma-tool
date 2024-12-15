# Run CARiSMA in a container via an SSH connection
The docker file in this directory allows you to create an ubuntu container on a remote machine that you can connect to via ssh and start CARiSMA to display the GUI on the connecting machine, where a display is attached. Performance depends on the network connection.

1. On the (remote) docker host: create an authorized_keys file in the directory, where you have the `Dockerfile`. The `authorized_keys` file contains the public SSH key(s) of user(s) that will be allowed to login to the container via their private SSH key(s).
2. On the (remote) docker host: build the image with `docker build -t carisma .`
3. On the (remote) docker host: run the container and optionally map some available docker host port (e.g. 2222) to the container's SSH port (22) with `docker run -it  -p 2222:22 carisma`
4. On the local host: connect via ssh (with X11forwarding) and run CARiSMA: `ssh -X -p 3000 root@<docker-host> /opt/carisma/carisma-launcher`
5. Enjoy CARiSMA

