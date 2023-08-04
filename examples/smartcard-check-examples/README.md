# UMLsec Locked Status Smartcard Check

carisma.check.smartcard.lockedstatus

This check analyzes UML state diagrams using the «locked-status» stereotype (cf. Ochoa et al. - Model-based Security Verification and Testing for Smartcards, 2011) by checking every state marked by «locked-status». 
These states may not have any outgoing transitions.

Annotate a model containing a state diagram with «locked-status» as necessary. 
After finishing the model, create a new CARiSMA analysis on the model. 
Add the Smartcard Locked Status Check to the used checks and click "Run" to start the analysis.

The Analysis Results View displays whether the check was successful. 
If any locked state has outgoing transitions, appropriate informations are displayed in the Analysis Results View.

# UMLsec Authorized Status Smartcard Check

carisma.check.smartcard.authorizedstatus

This check analyzes UML state diagrams using the «authorized-status» stereotype (cf. Ochoa et al. - Model-based Security Verification and Testing for Smartcards, 2011) by checking every state marked by «authorized-status». 
Incoming transitions to these states must have Constraints (Guards) containing the permission string provided by the tag permission of the authorized state.

Annotate a model containing a state diagram with «authorized-status» as necessary and set the permission tag value. 
After finishing the model, create a new CARiSMA analysis on the model. 
Add the Smartcard Authorized Status Check to the used checks and click "Run" to start the analysis.

The Analysis Results View displays whether the check was successful. 
If any incoming transition of an authorized state doesn't contain the appropriate permission string in its guard, the violation is displayed in the Analysis Results View.