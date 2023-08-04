# Role-Centric Attribute-Based Access Control (RABAC)

This plugin performs the analysis of models annotated with stereotypes from the RABAC profile, which is is provided by the plugin carisma.profile.umlsec.rabac. 
It consists of two checks, which rely on each other :

## Create transformation input

The output of this check is required for the analysis of a RABAC model. 
It displays a GUI with which the user can define the sessions for each user and the values of all attributes that are contained in the given model. 
Depending on which OCL syntax is used in the filters, three basic attribute types are supported:
- Strings
- Numbers
- Sets (must be notated as {A,...})

**Note**: Press Enter to change a value while the text field is selected. 
Multiple configurations can be generated and then be applied in the next step. 
If the syntax in the stereotype tags is invalid, the check will fail with an according error message added to the "Analysis Results" view.

## Use transformation input

This check analyses the RABAC constraints from stereotypes abacRequire in the given model based on a provided configuration file. 
The sessions must match with the available users and the dynamic separation of duty needs to be satisfied as well. 
The details are printed into the report file and include the allowed operations for each user. 
If state diagrams use constrained transitions, their contained paths are computed and displayed by using the target state names.
