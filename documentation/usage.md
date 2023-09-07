# Create a new Papyrus Project

## Requirements
- Install Eclipse
- Install Papyrus for Eclipse
- Install CARiSMA for Eclipse


## Creating a model
1. Create a new Papyrus project. Alternatively use an existing project as container for your models and select *Papyrus Model* in the dialog (*File* → *New* → *Papyrus Model*).
2. Select *UML* as the model type.
3. Enter a name for your Papyrus project.
4. Enter the *Diagram Name* and select the diagram type you want to create. Check that you want to load an UML model with basic primitive types as a template - this will make modeling easier for you. Optionally, you can directly register the the UMLsec profile for your project, by clicking ”Browse Registered Profiles”.
5. Choose the UMLsec profile and click *OK* and afterwards *Finish* to open the new empty diagram.


## Importing the UMLsec profile after model creation
1. If the model is open, close the *.di* file of your project.
2. In the *Project Explorer* open the *.uml* file of your model.
3. Select the <Model> element in the UML Model Editor.
4. Select the menu item *UML Editor* → *Package* → *Apply Profile*.
5. Select the *UMLsec* profile in the appearing window.
6. Click *Add* to apply the profile and *OK* to confirm your decision.
7. Save your project.
8. If you re-open your model in Papyrus, the UMLsec profile is added and its stereotypes may be applied.

## Analyzing a model with CARiSMA
1. You created a model, applied the UMLsec profile and stereotypes/tags.
2. Click *File*, then *New* and then *Other*.
3. Choose *CARiSMA* and then *Analysis*.
4. Select a model to be analyzed.
5. Select the *.uml* file of your created model.
6. Open the created *.adf* file and press the ”+” button.
7. Choose a suitable check for the model you created.
8. Run the check and obtain the result.
9. If the check fails, create a report.
10. View the report and obtain the results.
11. Correct the model according to the check.
12. Run the check again and observe if it is correct.

# Preference Settings

## CARiSMA Preference Settings
The CARiSMA settings can be customized by the user. 
Open Window → Preferences and choose CARiSMA on the left side, so the CARiSMA preference page is shown

### Open model settings
Depending on the analysis settings, the defined model may change during the execution. 
So the user may like to edit or review the used model. 
The *Open a Model with* settings in CARiSMA's main preference page specify the editors to be used when opening a model using the *Open Model* button. 

### Editor selection combo box
If you want to select a different editor for each analysis file, select the *Editor selection combo box* option.
By clicking on the *Open Model* button in Analysis Editor, the editor chosen in *Associated Editor* combo box (e.g. Eclipse's TextEditor, Papyrus, etc.) will appear. 
The selected editor will be applied just for the current opening. 
If the analysis is saved, the *Associated Editor* selection will be saved too. 

### Priority List
To define an ordered list of editors for automatically opening of a model, select this option.
Use the *Select ModelType* combo box to define editor priority lists for the different model types (e.g. UML2, BPMN2). 
Use *Add* or *Remove* buttons to add currently available editors into the ordered list or remove an editor from it. 
There must be at least one entry. 

The first entry in the list has the highest priority. If opening the model with this entry fails, the next entry will be taken automatically. There are currently following editors supported:
* **Default Eclipse Editor** is not a specific editor. With this option, the editor which is selected by eclipse by default is chosen.
* **Text Editor** opens the model file as text.
* **UML Model Editor** opens an uml model with its default editor.
* **Papyrus** search for the graphical file of the model (di file extension) in the same folder as the model itself, and if it exists it will be opened with the Papyrus editor.
* **ECORE Editor** opens the model file in a basic EMF ecore editor.

# BPMN2 Guides

## The OCL Library

### Creating an OCL Library File
First you need to create a model file. 
Therefore open the Creation Wizard by right clicking on your Workspace, select "New" then "Other" or use Hotkey CTRL + N. 
Following screen should appear. 
Select the OCL Model.
To manage multiple ocl constraints it is necessary to declare the "Library" Object as root element. 
In further releases this step might not be performed anymore.

### Using an OCL Library File
After creating an OCL Library File it is possible to add your individual ocl constraints to the library. 
Later on, these constraints can be deleted and modified as well. 
To add a constraint open the model file and add a constraint by right clicking on the library root element.
To modify an OCL constraint it is necessary to open the properties view (Select Menu "Window" then "Show View").

# Online Help
Additional help on the CARiSMA-Tool can be found in the online help, when CARiSMA is already installed.
1. Open Eclipse
2. Click *Help*.
3. Choose *Help Contents*.
4. A page will open with different contents for different installations.
5. Select the *CARiSMA* help.