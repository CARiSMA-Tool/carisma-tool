# Features

The CARiSMA core is independent from any particular modeling language. It is just based on the Eclipse Modeling Framework and thus supports each language built upon that framework, including general languages such as UML2 and domain specific languages such as BPMN2.

## UML2 Support

Due to the fact that CARiSMA has its origin in the UMLsec tool, it supports UML. The installation of the UML2 feature is therefore mandatory. Currently most compliance, risk, and security checks are available for UML2 models. Due to the broad acceptance of the Eclipse UML2 plugin used by CARiSMA, it can be tightly integrated with many tools.

We have tested and recommend the analysis of UML models that have been created with

- Papyrus 6.4.0

In the past, the analysis of UML models created with

- Rational Software Architect 8.0.1 - 8.0.3 (using the export filter to UML2)

worked as well.

## BPMN2 Support

The Business Process Model and Notation (BPMN) is a graphical representation for specifying business processes. The control of compliance, risk, and security properties is a fundamental paradigm of business workflows. An extension of CARiSMA offers Checks that perform custom security analyses on BPMN2 models.

We have tested and recommend the analysis of BPMN2 models that have been created with

- Eclipse BPMN2 Modeler

In the past, the analysis of UML models created with

- Yaoqiang BPMN Editor 2.0.109

worked as well.

## Evolution Support for UML2

With respect to long living software systems, it is often necessary to analyze whether evolution has an impact on compliance, risk, or security requirements 1). The evolution feature provides the UMLchange profile that allows developers to define possible evolutions within a UML model. Evolution-aware checks can then analyze whether the evolution has an impact on the security.

## List of Plugins


| Plugin  | Core/Extension | Context | Author | Description |
| ------------- | ------------- | ------------- | ------------- | ------------- |
| carisma.core  | Core  |   | Sven Wenzel, Lidiya Kaltchev  | The core tool (GUI, etc.)  |
| carisma.core.io  | Core  |Content Cell  | SecSE group, TU Dortmund  |   |
| carisma.ocl  | Extension  | Bachelor Thesis  | Marcel Michel  | Re-usable methods for the ocl support  |
| carisma.ocl.library.*  | Extension  | Bachelor Thesis  |Marcel Michel  | Model to manage ocl expressions  |
| carisma.check.oclcheck  | Extension  | Bachelor Thesis |Sebastian Haronski, Marcel Michel  | Queries emf models with a defined ocl constraint  |
| carisma.check.dummy  | Core |   |Sven Wenzel  | Dummy plugin that prints the model content into the report  |
| carisma.check.template  | Core  |   |Benjamin Berghoff  | Template for creating UMLsec plugins  |
| carisma.modeltype.uml2  | Core  |   |Sven Wenzel, Daniel Warzecha  | UML2 meta model  |
| carisma.profile.umlsec  | Core |  | Sven Wenzel, Daniel Warzecha  | UMLsec profile  |
| carisma.check.activity2petrinet  | Extension  | Bachelor Thesis  | Kubi Mensah  | Check that converts an activity diagram to a petrinet for further processing in ProM  |
| carisma.check.activitypaths  | Core  |   | Moussa Oumarou  | Check that prints out all possible paths in a activity diagram  |
| carisma.check.smartcard  | Extension  |   | Klaus Rudack  | Checks for smartcard specific properties  |
| carisma.check.statemachinepaths  | Extension  |   | Klaus Rudack  | Check that prints out all possible paths in a activity diagram |
| carisma.check.staticcheck  | Core  |    | Daniel Warzecha/Sven Wenzel  | Checks for the static UMLsec stereotypes  |
| carisma.profile.umlchange  | Extension  |    | Daniel Warzecha/Sven Wenzel  | UMLchange profile  |
| carisma.evolution  | Extension  |    | Daniel Warzecha/Sven Wenzel  | Evolution Support for CARiSMA  |
| carisma.evolution.uml2  | Extension  |   | Daniel Warzecha/Sven Wenzel  | UML2 specific evolution support for CARiSMA  |
| carisma.evolution.uml2.umlchange  | Extension  |   | Daniel Warzecha/Sven Wenzel  | UMLchange Parser Extension  |
| carisma.evolution.uml2.umlchange.ui  | Extension  |   |    |    |
| carisma.evolution.emfdelta  | Extension  |   | Johannes Kowald  | EMFcompare-based Delta Description Generator  |
| carisma.check.smartcard.evolution  | Extension  |   | Daniel Warzecha  | Smartcard Evolution Check  |
| carisma.check.staticcheck.evolution  | Extension  |   | Daniel Warzecha/Sven Wenzel  | Smartcard Evolution Check  |
| carisma.check.emfdelta  | Extension  |    | Johannes Kowald  | Check that prints out the EMFcompate-based Delta Description  |
| carisma.check.modelexporter  | Extension  |   | Benjamin Berghoff  | Exports Delta Models and Modified Models  |
| carisma.modeltype.bpmn2  | Extension  | Bachelor Thesis  | Marcel Michel  | Bpmn 2.0 meta model  |
| carisma.modeltype.bpmn2.extended.*  | Extension  | Bachelor Thesis  | Marcel Michel  | Bpmn 2.0 meta model with addiontional entity classes  |
| carisma.modeltype.bpmn2.extension.*  | Extension  | Bachelor Thesis  | Marcel Michel  | Meta model which holds the additional informations for the extended model  |
| carisma.check.bpmn2.ocl  | Extension  | Bachelor Thesis  | Marcel Michel  | Queries bpmn2 models with ocl expressions  |
| carisma.check.bpmn2.marisk  | Extension  |   | Sebastian Haronski  | MaRisk support for bpmn2  |
| carisma.check.bpmn2.dummy  | Extension  | Bachelor Thesis  | Marcel Michel  |    |
| carisma.check.idschecks  | Extension  | Bachelor Thesis  | Alexander Peikert  |    |
| carisma.check.parametertest | Core  |   | SecSE group, TU Dortmund  |   |
| carisma.check.rabac  | Extension  | Bachelor Thesis  | Patrick Hoffmann  |     |
| carisma.check.requirescheck  | Extension  |   |   |    |
| carisma.check.sequencediagramcrypto  | Extension  |   |    |    |
| carisma.profile.enc  | Extension  |   |    |    |
| carisma.profile.rabac  | Extension  | Bachelor Thesis  | Patrick Hoffmann  |    |
| carisma.profile.umlsec4ids  | Extension  | Bachelor Thesis | Alexander Peikert  |    |
| carisma.ui.console  | Core  |   | SecSE group, TU Dortmund  |    |
| carisma.ui.eclipse  | Core  |   | SecSE group, TU Dortmund  |    |
| carisma.ui.eclipse.descriptor  | Core  |   |  SecSE group, TU Dortmund  |    |




