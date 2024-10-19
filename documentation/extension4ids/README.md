# Extension4IDS Examples

This directory contains the exmaples of the checks introduced for the Extension4IDS profile. There are two static analysis checks: the **Usage Control Check** and the **Transfer Process Protocol (TPP) Check**. These checks provide partial support for analyzing security requirements within the [IDS framework](https://docs.internationaldataspaces.org/ids-knowledgebase/ids-ram-4), ensuring that data sovereignty and secure data transfers are maintained.

**[Usage Control Check](./usage-control/README.md)**: The **Usage Control Check** analyzes deployment diagrams to ensure that data shared between entities is controlled and used according to predefined policies. This check focuses on verifying that data exchanges are enforced by secure IDS connectors.

**[Transfer Process Protocol (TPP) Check](./transfer-process-protocol/README.md)**: The **Transfer Process Protocol (TPP) Check** analyzes sequence diagrams to ensure the correct sequence of messages between a data provider and a data consumer. This check ensures secure and standardized data transfers within the IDS ecosystem.
