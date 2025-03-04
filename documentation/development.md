# Setting up CARiSMA for development
1. Install [Eclipse](installation.md#eclipse-modeling-tools)
2. Install [BPMN2 Modeler](installation.md#bpmn2-modeler) **(The BPMN2 Modeler is not compatible with Eclipse versions starting from 2024-06)**
3. Install [Papyrus](installation.md#papyrus)
4. If you already installed CARiSMA, delete the local installation.
5. Import the CARiSMA Tool from the [git repository](https://github.com/CARiSMA-Tool/carisma-tool).
6. In your workspace, go to the "plugins" and "test" folder and close all projects containing "bpmn2", "ocl" and "rabac".
7. Open a new Terminal 
8. Move to the base directory of the carisma-tool
9. Execute *mvnw.cmd clean install* on Windows or *./mvnw clean install* otherwise
10. Refresh the carisma-tool project.
11. All errors should be resolved.

To start your working copy of CARiSMA, right click on a plugin project, select "Run As" -> "Eclipse Application".

Sometimes it can happen that the target platform configuration is broken. To fix this issue, you need to navigate *Preferences* → *Plug-In Development* → *Target Platform*. In the *Target Platform* settings, you can specify the plugins and libraries that should be included in your target platform configuration. You can add, remove, or modify the entries as necessary to ensure that you have the correct dependencies.


# Creating a Plugin Project containing CARiSMA Checks
1. Create a new Plug-in Project: New → Other → Plug-in Development → Plug-in Project
2. Enter your plugin name. The convention is **"carisma.check.[pluginName]"**.
3. Set output folder to `target`.
5. Click *Next*.
6. Enter the metadata for your plugin.
7. Click *Next*.
8. Select the *CARiSMA UML2 Check Template*.
9. Click *Next*.
10. Fill out the missing fields.
    * *Check ID* is a unique ID for your Check. You may choose the package name as the Check ID.
    * *Package Name* is the name of your Package.
    * *Class Name* is the name of your Java class implementing the check.
    * *Check Name* is the name of the check as part of the CARiSMA project.
    * *Check Description* is the description of the CARiSMA check.
    * *Publisher* should be your name or the name of your company.
    * *Target Model Type* is the Model-Type your check will work with.
11. Click *Next*.
12. If you want to create a preference page containing check-wide settings, check the corresponding option *Create a Preference Page*.
13. Select a *Page name*. *Class Name* and *Package Name* are the java Class Name/Package Name.
14. Click *Finish* to create your first CARiSMA check!

# The CARiSMA Tool Architecture
CARiSMA is implemented as an Eclipse-based plugin while itself has a plugin architecture to be easily extensible.
An excerpt of the contents of the main plugin *carisma.core* can be seen in the figure below.

![CARiSMA Core](images/architecture-tool.png)

The main class *Carisma* is registered as an Eclipse plugin and provides access to the various structures, serving as the controller for the different components of the tool.

An Analysis is executed on a Model of a certain ModelType registered within the ModelTypeRegistry. 
The models themselves are provided by the ModelManager and the ModelLoader for the corresponding ModelType.

CARiSMA executes analyses consisting of various CheckReferences to CheckDescriptors. 
These descriptors define the CarismaChecks that have been registered with the CheckRegistry. 
Each CarismaCheck may have any number of CheckParameters defined, which has a certain ParameterType. 
Checks may define pre- and post-conditions to use in combination with a blackboard (not pictured) to exchange information between them.


The AnalysisHost interface provides access to this blackboard, as well as methods for displaying the results of an Analysis in the AnalysisResultsView. 
The Analyzer is the main CARiSMA implementation of the AnalysisHost interface.

## Getting started developing with CARiSMA

### Introduction

While developing CARiSMA, we built some useful classes to ease development. In this section, we will present these classes.

### CARiSMA

#### Analysis Host

The interface `AnalysisHost` offers method stubs for the creation of structured reports for your analysis and for the exchange of data between different checks.

#### The ModelTypeRegistry

The model type registry manages the different types of models that are supported by CARiSMA. 
In order to have a more precise definition of which model types UMLsec2 supports, each model type is represented by a seperate plugin *carisma.modeltype.XXX* where *XXX* defines the model type, e.g. *uml2* or *bpmn2*. 
These plugins refer to the EMF-related plugins that implement the *model type* (they can also define the model type directly) and they provide routines for loading models of that type.

#### The ModelManager

The model manager is used to load models.

### UML

#### StateMachinePaths

This class gets all the possible paths through a state machine.
It is also able to get all state machines out of a model.
The most important method is `ArrayList<ArrayList<Element>> getPaths(final StateMachine stateMachine, final AnalysisHost analysisHost, final boolean transitions)`.
This method needs a state machine where it should get the paths out of, an AnalysisHost (null if you have none), and a boolean whether you also want the transitions occur in the result.

#### UMLStateMachineHelper

This class contains methods to easily manage a state machine.
Methods to get transitions to a given constraint or a target state.

#### ActivityDiagramManager

This class gets all possible paths out of an activity diagram.
The constructor `public ActivityDiagramManager(final Package model, final AnalysisHost host)` needs the model and an AnalysisHost for report (null if you have none).
Afterwards you can get all paths with `List<List<Element>> getAllPaths()`.

#### UMLHelper

This class contains convenience methods to easily access UML2 models.
For example, `getAllElementsOfType(Element inThisElement, Class<T> type)` returns all elements of a given type, `getElementByName(final Model model, final String adequatelyQualifiedName)` returns the element in the model specified by the name.

#### UMLDeploymentHelper

This class contains methods to easily manage a deployment diagram.
Methods to get specific elements or just a subset of them are provided here.

#### StereotypeApplication

A StereotypeApplication manages a Stereotype.
This class provides easy access to a Stereotype's attributes as its name or owner.

#### TaggedValue

A TaggedValue provides easy access to a Stereotypes's tags.
You can get the value (or values if the tag is multi-valued) of its corresponding Stereotype etc.

### UMLsec

#### UMLsecUtil

This class provides easy access to the profile's stereotypes and to applications of those.
For example, with `getStereotypedElements(final Package pkg)` you get all elements in the package to which a UMLsec Stereotype is applied, or with `getStereotypeApplication(final Element element, final UMLsec stereo)` you get the StereotypeApplication of the given Stereotype whenever the Stereotype is applied to the Element.

### UMLchange

#### UMLchangeUtil

This class provides easy access to the profile's stereotypes and to applications of those.
For example, `isProfileApplied(Package pkg)` returns true if the UMLchange Profile is applied to the given Package otherwise false.
Like `UMLsecUtil`, methods to get Stereotyped Elements, StereotypeApplication of Stereotypes etc. are provided here.

### BPMN

#### BPMN2Helper

This class contains convenience methods to easily access BPMN 2 models.
For example, `getAllElementsOfType(DocumentRoot rootElement, Class<T> type)` returns all elements of a given type, `getElementById(EObject rootElement, String id)` returns the element in the model specified by the Id.
Furthermore this class offers the ability to check if a defined model was created by the Yaoqiang Editor `isYaoqiangModel(String inFilePath)`.
If a Yaoqiang model is used during an Analysis the conversation is done automatically.
For a better integration and illustration the CARiSMA group modified the standard Yaoqiang Editor to easily mark security relevant elements.
If a critical element is identified the method `setWarningFlagToBpmn2Element(Bpmn2Element element, String description, Color color)` adds a flag with a defined color to the model element. To remove all warning flags in the model the method `clearAllWarningAndInfoFlags()` can be used.

# The Check Mechanism

The CARiSMA tool provides a mechanism that allows you to implement multiple checks in a single Eclipse Plugin.
The CARiSMA tool itself is only a host for the checks and coordinates their execution.
The mechanism is shown below.

![Check Mechanism](images/check-mechanism.png)

## Implementing CarismaChecks

Creating a CARiSMA Check can be done automatically by using an Eclipse Template Project. CARiSMA checks need to extend the AnalysisCheck extension point and implement the CarismaCheck interface.

### Using the AnalysisCheck Extension Point

In the CARiSMA tool, checks are registered by the CheckRegistry.
To be registered, a plugin containing CARiSMA checks needs to extend the AnalysisCheck extension point.
This extension point lets the check developer define checks.

Each defined check has to be provided with certain meta information about it, which is displayed in the analysis editor.
The information that has to be provided is

- The ID, Name, Description and Publisher of the check
- The Model type to which this check can be applied to
- The Java class implementing this check

Optionally, some magic keys to identify models can be defined. CARiSMA looks for these keys in the model files.
If any key is found, a set of recommended checks is added to the list of checks in an analysis.

Check parameters are defined using this extension point.

There are seven parameter types: String, Boolean, Integer, Float, InputFile, OutputFile and Folder, which declares a folder to store the check output in.
Depending on the type, the analysis editor provides appropriate methods for setting parameter values.

After defining a check and its parameters, the CheckRegistry registers the check with CARiSMA on startup.
The meta information is stored in a CheckDescriptor, while the parameter information is stored in a CheckParameterDescriptor.
Registered checks are available to use in an analysis using the Analysis Editor.

Once an analysis is started, it receives CheckReferences which contain the set parameter values.

### The CarismaCheck Interface

A class implementing the CarismaCheck interface has to implement a single method.

`boolean perform(Map parameters, AnalysisHost host)`

This method is called by CARiSMA when a check is executed.
The check's parameters are given in a mapping from the parameter id to the corresponding parameter.
The given host can be used to access the analysed model, log check results to the AnalysisResultsView or a report produced by the analysis, and to access the blackboard provided by CARiSMA, which is used for exchanging data between checks.

### Pre-/Postconditions

CARiSMA Checks can be further described with pre- and postconditions.
While preconditions define which data is necessary for the check to be executed, postconditions tell CARiSMA of the data the check provides after successful execution.
This mechanism works in tandem with a blackboard which enables inter-check data exchange.

Before the execution of each check in an analysis, CARiSMA verifies that the preconditions imposed by the check are present in the blackboard.
If any precondition is missing, the analysis stops with an appropriate error.

With postconditions, CARiSMA ensures that the data the check says it provides is present in the blackboard.
If any postcondition is missing, CARiSMA again stops the analysis while producing an error.

#### Defining pre-/postconditions

Pre- and postconditions can be defined like adding parameters to a check.
Right-click on a defined CARiSMA check and select *New* → *precondition/postcondition* from the menu.
The only thing left to do is setting a value for the required or provided key of the condition.
This value is used by CARiSMA to search in the blackboard.

Defined pre- and postconditions are shown in the Analysis Editor along check parameters.

#### Using the blackboard to store and retrieve data

The blackboard can be accessed via the AnalysisHost interface.
`putToRegister(String registerName, Object data) throws RegisterInUseException`

Stores data in the blackboard if the key isn't already in use.
`Object getFromRegister(String registerName) throws RegisterNotInUseException`

Retrieves data from the blackboard if the key is in use.
`boolean isRegisterInUse(String registerName)`

Checks if the register is already in use.
`Object removeFromRegister(String registerName) throws RegisterNotInUseException`

Removes data from a register with the given key if it is in use.

### Generating Check Output

Checks are provided with various output channels. The Analyzer, the CARiSMA implementation of the AnalysisHost interface, allows checks to output messages to the AnalysisResultsView.

An AnalysisResultMessage given to the AnalysisHost consists of a StatusType (`INFO`, `WARNING`, `ERROR`) to prefix messages in the view with an appropriate icon, and the message to print.

Apart from the view, messages can also be sent to be printed with a report.
This report can be specifically generated by using the context menu in the AnalysisResultsView.
We recommend using the report for more detailed messages regarding the check.


# UML Profile Creation Using Papyrus for CARiSMA

## Prerequisites

- **Eclipse Modeling Tools**: Version 2024-03 (4.31.0)
- **Papyrus**: Version 6.6.0.202401120925
- Ensure you are working on the workspace that contains the `carisma-tool` project.

---

## 1. Create a Papyrus Profile Project

1. Go to `File > New > Project > Papyrus Project`.
2. Under **Software Engineering**, select `Profile`.
3. Name the project: `carisma.profile.<profilename>`.
    - Model File Name: `<profilename>`
    - Root Model Element Name: `<profilename>`
    - Representation Kind: `Profile Diagram`
4. Click `Finish`.
5. Open `<profilename>.profile.di`.
6. Select `Create View`.
    - Double-click on the element with the name `<profilename>` and select `Profile Diagram`.
7. In the **Profile Diagram**, make necessary extensions:
    - To add a new stereotype to an **ActivityPartition** element in an Activity Diagram:
      - Drag and drop `Import Metaclass` from the **Nodes** palette.
      - Search and select `ActivityPartition`.
      - Drag and drop `Stereotype` from the **Nodes** palette and name it (e.g., `test`).
      - Use `Extension` from the **Edges** palette to connect the stereotype to the metaclass.
    - To add tag values:
      - Click on the stereotype (e.g., `test`), navigate to the **Properties** tab.
      - Select `UML`, then under **Owned Attribute**, add attributes (name, label, type, multiplicity, etc.).
8. Save the profile and provide dynamic definition details (date, author, version). Update the dynamic definition with every change.

---

## 2. Create Ecore Model from the Profile

1. Right-click on the UML file under `<profilename>.profile` and select `New > Other > Eclipse Modeling Framework > EMF Generator Model`.
2. Set the parent folder and file name: `<profilename>.profile.genmodel`.
3. Select `UML Model` as the Model Importer and click `Next`.
4. Ensure the Model URI is correct, click `Load`, and then `Next`.
5. Under **Root Packages**, select `<profilename>` package and set the file name to `<profilename>.profile.ecore`.
6. Under **Referenced generator models**, select `Ecore`, `Types`, and `UML` (if present).
7. Click `Finish`.
8. Right-click the generated `.genmodel` file, open it with `EMF Generator`, right-click the root package `<profilename>`, and select `Generate Model Code`.
9. Refactor the generated `src` package to `gen-src` and rename packages to `carisma.profile.<profilename>`.
10. Create a new `src` package and set it up as a source folder in the Java Build Path.

---

## 3. Integrate the New Profile in CARiSMA

1. Under `src`, create:
    - `Activator.java`
    - `<ProfileName>.java`
    - `<ProfileName>Util.java`
    - Refer to a similar structure from the CARiSMA GitHub page.
2. Open `MANIFEST.MF` in the `META-INF` folder and set up:
    - **Overview Tab**: Set `ID`, `Version`, and select `Activator.java` under Activator.
    - **Dependency Tab**: Add required plug-ins:
      - `org.eclipse.core.runtime`
      - `org.eclipse.emf.ecore`
      - `org.eclipse.uml2.types`
      - `org.eclipse.uml2.uml`
      - `carisma.core`
      - `carisma.modeltype.uml2`
      - `org.eclipse.papyrus.uml.extensionpoints`
    - **Runtime Tab**: Add packages in `gen-src` under Exported Packages.
    - **Extensions Tab**:
        - `org.eclipse.uml2.uml.generated_package`
            - Right-click and select `New > profile`.
            - URI: `http://www.umlsec.de/profiles/UMLsec/<profilename>`
            - Location: `platform:/plugin/carisma.profile.umlsec.<profilename>/profile/<profilename>.profile.uml#<xmi_id>`
        - `org.eclipse.emf.ecore.uri_mapping`
            - Right-click and select `New > mapping`.
            - Source: `pathmap://UMLsec/<profilename>.uml`
            - Target: `platform:/plugin/carisma.profile.umlsec/<profilename>/profile/<profilename>.profile.uml`
        - `org.eclipse.papyrus.uml.extensionpoints.UMLProfile`
            - Right-click and select `New > profile`.
            - Name: `UMLsec.<profilename>`
            - Path: `pathmap://UMLsec/<profilename>.uml`
            - IconPath: Path to the CARiSMA logo file.
        - `org.eclipse.emf.ecore.generated_package`
            - URI: `http://www.umlsec.de/profiles/UMLsec/<profilename>.ecore`
            - Class: `carisma.profile.umlsec.<profilename>.<Profilename>Package`
            - GenModel: `profile/<profilename>.profile.genmodel`
3. Edit the `build.properties` file:
    ```
    source. = gen-src/,\
    src/
    output. = target/classes/
    bin.includes = META-INF/,\
    .,\
    plugin.xml,\
    profile/,\
    plugin.properties,\
    src/,\
    gen-src/
    src.includes = profile/
    ```
4. Import the project into the plugin directory of the CARiSMA tool project.
5. Edit `pom.xml` in the CARiSMA tool project and add:
    ```xml
    <module>plugins/carisma.profile.{profile_name}</module>
    ```
6. Build the CARiSMA tool project and run it as an Eclipse application.
