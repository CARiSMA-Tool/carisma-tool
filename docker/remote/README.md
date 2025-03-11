# Run CARiSMA in a container via an SSH connection
The docker file in this directory allows you to create an ubuntu container on a remote machine that you can connect to via ssh and start CARiSMA to display the GUI on the connecting machine, where a display is attached. Performance depends on the network connection.

1. On the docker host: build the image with `docker build -t carisma .`
2. On the docker host: create an `authorized_keys` file that contains the public SSH key(s) of user(s) that shall be allowed to login to the container via their private SSH key(s).
3. On the (remote) docker host: run the container, mount the `authorized_keys` file to  `/root/.ssh/authorized_keys` and optionally map some available docker host port (e.g. 2222) to the container's SSH port (22). This could look like: `docker run -p 2222:22 -v ./authorized_keys:/root/.ssh/authorized_keys carisma`
4. On the local host: connect via ssh (with X11forwarding) and run CARiSMA: `ssh -X -p 2222 root@<docker-host> /opt/carisma/carisma-launcher`
5. Enjoy CARiSMA

