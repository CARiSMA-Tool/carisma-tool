# CARiSMA

<img align="right" width="450" height="347" src="documentation/images/carisma-home.png">

Modeling offers an unprecedented opportunity for high-quality critical systems development that is feasible in an industrial context. CARiSMA enables you to perform:

* compliance analyses,
* risk analyses, and
* security analyses

of software models. CARiSMA is an acronym for **C**ompli**A**nce, **Ri**sk, and **S**ecurity **M**odel **A**nalyzer.

Since CARiSMA is a reimplemented variant of the former UMLsec tool it natively supports UML models. Due to its EMF-based implementation CARiSMA can also support domain-specific modeling languages such as BPMN.

CARiSMA is fully integrated into Eclipse and can thus become part of the modeling tool of your choice including but not limited to Papyrus MDT, IBM Rational Software Architect, and many others.

A flexible plugin architecture makes CARiSMA extensible for new languages and allows users to implement their own compliance, risk, or security checks.

## Installation
See [Installation Guide](documentation/installation.md) for how to install CARiSMA and its extensions.

## General Usage
See [Usage Guide](documentation/usage.md) for information on how to use CARiSMA.


## Specific checks
### Static Checks
See [Static Checks](examples/static-check-examples/README.md) for the specific checks.
### UMLsec4IDS
See [UMLsec4IDS](examples/umlsec4ids/README.md) for the specific checks. 
### Activity2PetriNets
See [Activity2PetriNet](examples/activity2pertinet/README.md) for the specific checks. 
### Smartcard Checks
See [Smartcard](examples/smartcard-check-examples/README.md) for the specific checks. 
### The OCL Check Plugin
See [OCL](examples/ocl-check-examples/README.md) for the specific checks. 
### RABAC
See [RABAC](examples/rabac/README.md) for the specific checks. 


## Development
If you want to extend CARiSMA, consult the [Development Guide](documentation/development.md).

## Video Documentation
There are multiple different screencasts available on the CARiSMA-Tool: 
* [Screen cast for the ESEC/FSE 2017 tool demonstration track](https://www.youtube.com/watch?v=b5zeHig3ARw)
* [Install CARiSMA in Topcased](https://rgse.uni-koblenz.de/carisma/videos/Install_Carisma_to_Topcased.avi)
* [Modeling with UMLsec stereotypes](https://rgse.uni-koblenz.de/carisma/videos/SecureChangeReview2012-DemoScript_Step1.avi)
* [Analyzing a model annotated with UMLsec](https://rgse.uni-koblenz.de/carisma/videos/SecureChangeReview2012-DemoScript_Step2.avi)
* [Analyzing a potential evolution of a model](https://rgse.uni-koblenz.de/carisma/videos/SecureChangeReview2012-DemoScript_Step3.avi)
* [General usage](https://rgse.uni-koblenz.de/carisma/videos/dummy_check.avi)
* [Example run with a failing security check](https://rgse.uni-koblenz.de/carisma/videos/check_fails.avi)
* [Example run with a successful security check](https://rgse.uni-koblenz.de/carisma/videos/check_ok.avi)
* [Correcting a model according to a failed report](https://rgse.uni-koblenz.de/carisma/videos/correcting_model_wrt_to_report.mp4)
* [Applying the UMLsec profile and Stereotypes](https://rgse.uni-koblenz.de/carisma/videos/apply_profile_and_stereotypes.mp4)

## Contact / Team

If you find bugs, please use GitHub's issue tracker.

For other questions contact: [Research Group Software Engineering at University of Koblenz](https://www.uni-koblenz.de/de/informatik/ist/juerjens)

Contact:
* Jan Jürjens
* Julian Flake
* Sven Peldszus

Further developers and contributors:
* Sven Wenzel
* Daniel Poggenpohl, né Warzecha
* Benjamin Berghoff
* Jens Bürger
* Lidiya Kaltchev
* Johannes Kowald
* Kubi Mensah
* Marcel Michel
* Alexander Peikert
* Klaus Rudack


