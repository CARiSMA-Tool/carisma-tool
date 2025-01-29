# Prepare X server on local machine
## MacOS
1. Install XQuartz and configure XQuartz to Allow Connections (on first start)
  * Open XQuartz preferences 
  * Go to Security tab
  * Allow connections from network clients
  * Restart
2. Look up host IP address. In XQuartz run: `ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'`
3. Allow connections from your host IP. In XQuartz run: `xhost + <your_host_ip>`

## Linux
1. Make sure you have some X11 server running. If you're on wayland, you can use XWayland, which allows you to run X11 applications on wayland.
2. Allow X connections from the container to your local machine (host): Make sure you have the command `xhost` available at your local machine. You can try `which xhost`, which should tell you the path to your executable. The package you may need to install, depends on your distribution.
3. In a shell on the local machine run `xhost +`

## Windows
1. Install and configure XLaunch:
  * Select "Multiple windows" and set "Display number" to 0 and click "Next"
  * Choose "Start no client" and click "Next"
  * In "Extra settings" tick all boxes and cick "Next"
  * Click "Finish"
3. Open your Console and type `set DISPLAY=host.docker.internal:0.0` and press "Enter"

# Build the image and run the container

1. Build the Docker image. In your console run: 

```
    docker build -t carisma .
```
3. Run the Docker container with X11 forwarding:

```
    docker run -it \
      -e DISPLAY=$DISPLAY \
	  -v /tmp/.X11-unix:/tmp/.X11-unix \
	  carisma
```
4. Eclipse should start automatically
