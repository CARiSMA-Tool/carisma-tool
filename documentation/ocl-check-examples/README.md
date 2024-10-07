# The OCL Check Plugin

The OCL Check Plugin contains two Subplugins.
The Single OCL Check performs a single OCL query with a given context and query string, while the Multi OCL Check uses the OCL Library to enable multiple OCL queries.

### Single OCL Check

The Single OCL Check needs two parameters: the context and the query string. 
The context is the name of the represented object class, like 'Task', 'Element' or for context free queries 'context-free'. 
If the context fits to an object the second parameter, the query string, will be executed on the object and will return a boolean value.

### Multi OCL Check

The Multi OCL Check works similar to the Single OCL Check. 
To enable multiple OCL Queries the OCL Library is used as a parameter.

### Result View

The Result View shows the results of the executed constraints. 
Details, like the objects which violates a constraint, can be found in the log. 