# Setting up Eclipse for CARiSMA

## Eclipse Modeling Tools
- First, you need to install the Eclipse Modeling Tools. You can find it at : https://www.eclipse.org/downloads/packages/
- We successfully tested with Eclipse 2024-03
- If you have problems installing it, check out the installation guide at : https://wiki.eclipse.org/Eclipse/Installation

## BPMN2 Modeler (optional)
If you want to model business processes using BPMN models, you probably want to install the *BPMN2 modeler*. Use the Update Site :
https://download.eclipse.org/bpmn2-modeler/updates/2022-12/1.5.4-202212 .
1. Open Eclipse.
2. Navigate to *Help*.
3. Click *Install New Software*, then *Add* and use the Location mentioned above
4. Select *Eclipse BPMN2 Modeler* -> *BPMN2 Modeler - Diagram Editor*
5. Click *Next* and *Finish*
6. Wait for the installation process to finish and restart Eclipse.

## Papyrus
After installing the Eclipse Modeling Tools, you need to install Papyrus via the Update Site :
https://download.eclipse.org/modeling/mdt/papyrus/updates/releases/2023-12/ . 
1. Open Eclipse.
2. Navigate to *Help*.
3. Click *Install New Software*., then *Add* and Location mentioned above
4. At least, install ”Papyrus for UML” and ”Papyrus for UML Developer Resources”.
5. Click *Next* and *Finish*
6. Wait for the installation to finish and restart Eclipse.

## CARiSMA
Next, you are ready to install the CARiSMA tool. You can find the latest version here :
https://github.com/CARiSMA-Tool/carisma-tool .
Otherwise, you can also use the update site for the latest version: https://rgse.uni-koblenz.de/carisma/updatesite/ . To install CARiSMA, you need to follow the steps 1 - 4 from the Papyrus installation. Afterwards, you do the following:
1. Paste the update site into the *Location* field and name it.
2. Press *Add* and choose the features you want to install.
3. At least, install ”CARiSMA Checks for UML2".
4. Wait for the installation to finish and restart Eclipse.

Sometimes it can happen that the target platform configuration is broken. To fix this issue, you need to navigate *Preferences* → *Plug-In Development* → *Target Platform*. In the *Target Platform* settings, you can specify the plugins and libraries that should be included in your target platform configuration. You can add, remove, or modify the entries as necessary to ensure that you have the correct dependencies.
