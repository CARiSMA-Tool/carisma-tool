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
Please refer to the documentation in the [docker directory](../docker/) on how to run CARiSMA in a docker container.
