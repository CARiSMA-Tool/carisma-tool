# Setting up Eclipse for CARiSMA

## Eclipse Modeling Tools
- First, you need to install the Eclipse Modeling Tools. You can find it at : https://www.eclipse.org/downloads/packages/
- We successfully tested with Eclipse 2024-03
- If you have problems installing it, check out the installation guide at : https://wiki.eclipse.org/Eclipse/Installation

## BPMN2 Modeler (optional)
**(The BPMN2 Modeler is not compatible with Eclipse versions starting from 2024-06)**

If you want to model business processes using BPMN models, you probably want to install the *BPMN2 modeler*
1. Navigate to *Help* -> *Install New Software*, then click *Add* 
2. Enter the following URL into the "Location" field: https://download.eclipse.org/bpmn2-modeler/updates/2022-12/1.5.4-202212
3. Select *Eclipse BPMN2 Modeler* -> *BPMN2 Modeler - Diagram Editor*
4. Click *Next* and *Finish*
5. Wait for the installation process to finish and restart Eclipse.

## Papyrus
After installing the Eclipse Modeling Tools, you need to install Papyrus via the Update Site:
1. Navigate to *Help* -> *Install New Software*, then click *Add* 
2. Enter the following URL into the "Location" field: https://download.eclipse.org/modeling/mdt/papyrus/updates/releases/2024-06/
3. At least, install ”Papyrus for UML” and ”Papyrus for UML Developer Resources”.
4. Click *Next* and *Finish*
5. Wait for the installation to finish and restart Eclipse.

## CARiSMA
Next, you are ready to install the CARiSMA tool. There are two ways to install CARiSMA into your Eclipse:

### Installation via remote Update Site (recommended)
To install CARiSMA via the remote Update Site do the following:
1. Navigate to *Help* -> *Install New Software*, then click *Add* 
2. Enter the following URL into the "Location" field: https://carisma-tool.github.io/carisma-tool/
3. Press *Add* and choose the features you want to install. At least, install ”CARiSMA Checks for UML2".
4. As of 2024-08, do not install the BPMN2 features as the BPMN2 Modeler is not supported by Eclipse any longer.
5. Click *Next* and *Finish*
6. Wait for the installation to finish and restart Eclipse.

### Installation via Update Site ZIP file (alternative installation method)
To install CARiSMA via the Update Site ZIP file do the following:
1. Download the latest `carisma-updatesite.zip` release from https://github.com/CARiSMA-Tool/carisma-tool/releases
2. Navigate to *Help* -> *Install New Software*, then click *Add* 
3. Choose the downloaded ZIP file, optionally assign a name.
4. Press *Add* and choose the features you want to install. At least, install ”CARiSMA Checks for UML2".
5. Wait for the installation to finish and restart Eclipse.

# Setting up CARiSMA via Docker Container

## Documentation to start on MacOS

### 1. Install XQuartz 

### 2. Configure XQuartz to Allow Connections (on first start)
* Open XQuartz preferences 
* Go to Security tab
* Allow connections from network clients
* Restart

### 3. Look up host IP address  
In xQuartz run :
```
ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'  
```

### 4. Allow connections from your host IP:
In xQuartz run :
```
xhost + <your_host_ip>
```

### 5. Build the Docker image
In you console run :
```
docker build -t carisma-container .
```

### 6. Run the Docker container with X11 forwarding:
```
docker run -it \
    -e DISPLAY=<your_host_ip>:0 \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    carisma-container
```

### 7. Eclipse should start automatically

## Documentation to start on Linux

1. Make sure you have some X11 server running. If you're on wayland, you can use XWayland, which allows you to run X11 applications on wayland.
2. Make sure you have the command `xhost` available (try `which xhost`, which should tell you the path to your executable). The package you may need to install, depends on your distribution.
3. Allow connections from your host IP. In a shell run `xhost +`.
4. Build the Docker image: `docker build -t carisma-container`.
5. Run the Docker container with X11 forwarding and enjoy CARiSMA:

```
docker run -it \
    -e DISPLAY=:0 \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    carisma-container
```
