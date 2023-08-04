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