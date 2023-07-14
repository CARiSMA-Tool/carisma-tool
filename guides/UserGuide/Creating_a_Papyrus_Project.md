# How to create a Papyrus Project

## Requirements
- Install CARiSMA for Eclipse
- Install Papyrus for Eclipse


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
