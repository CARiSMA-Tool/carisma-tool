# Setting up Eclipse for CARiSMA

## Eclipse Modeling Tools
- First, you need to install the Eclipse Modeling Tools. You can find it at : https://www.eclipse.org/downloads/packages/
- If you have problems installing it, check out the installation guide at : https://wiki.eclipse.org/Eclipse/Installation

## BPMN2 Modeler
If you want to model business processes using BPMN models, you need to do the following steps:
1. Open Eclipse.
2. Navigate to *Help*.
3. Select the *Eclipse Marketplace*.
4. Search for the *Eclipse BPMN2 Modeler*.
5. Install the *Eclipse BPMN2 Modeler*.
6. Select at least the required features and confirm your choices.
7. Wait for the installation process to finish and restart Eclipse.


## Papyrus
After installing the Eclipse Modeling Tools, you need to install Papyrus via the Update Site : https://download.eclipse.org/modeling/mdt/papyrus/updates/nightly/master/ . At the moment, the Nightly Build of Papyrus is used since it is compatible with the latest Eclipse version. 
1. Open Eclipse.
2. Navigate to *Help*.
3. Click *Install New Software*.
4. Add a new repository
5. Paste the update site into the *Location* field and name it.
6. Press *Add* and choose the features you want to install.
7. At least, install ”Papyrus for UML” and ”Papyrus for UML Developer
     Resources”.
8. These can be found if you expand the features.
9. Wait for the installation to finish and restart Eclipse.


## CARiSMA
Next, you are ready to install the CARiSMA tool. You can find the latest version here :
https://github.com/CARiSMA-Tool/carisma-tool .
Otherwise, you can also use the update site for the latest version: https://rgse.uni-koblenz.de/carisma/updatesite/ . To install CARiSMA, you need to follow the steps 1 - 4 from the Papyrus installation. Afterwards, you do the following:
1. Paste the update site into the *Location* field and name it.
2. Press *Add* and choose the features you want to install.
3. At least, install ”CARiSMA Checks for UML2".
4. These can be found if you expand the features.
5. Wait for the installation to finish and restart Eclipse.

Sometimes it can happen that the target platform configuration is broken. To fix this issue, you need to navigate *Preferences* → *Plug-In Development* → *Target Platform*. In the *Target Platform* settings, you can specify the plugins and libraries that should be included in your target platform configuration. You can add, remove, or modify the entries as necessary to ensure that you have the correct dependencies.