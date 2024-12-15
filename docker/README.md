# CARiSMA in a docker container

There are two ways to run CARiSMA in a docker container. See the corresponding Dockerfile and instructions in:

* [local](local/), if you have docker installed on your local machine and a display is attached to your local machine. This makes use of direct X11 forwarding, or 
* [remote](remote/), if you have docker installed on a remote machine, you want to connect via SSH to that remote machine and want to use CARiSMA on your local machine where you have a display attached. This makes use of SSH's X11 forwarding feature.
