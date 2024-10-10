# UMLsec4IDS Examples

Here, you can find examples and guides for the different checks that are related to security concepts of the [International Data Space](https://internationaldataspaces.org).
The IDS is a virtual data space that ensures trust within the whole system.
Each participant is evaluated and certified before he can take part within this data ecosystem, through a certified connector, which guarantees his identity at all time.
Another target for the International Data Space is that data sovereignty is ensured at all time, so a data owner does not loose the ownership for the data he provides.
Participants can sell and buy data.
Before each transaction, a usage contract is accepted by both parties which states what the consumer is allowed to do with the data.

There are 6 different security concepts already implemented in CARiSMA.

[Data Access Control](data-access-control/README.md) grants or restricts access to a certain (data) resource.
There are multiple types of access control, but the International Data Space uses the Attribute-Based-Access-Control (ABAC) approach.
To access a resource, a consumer needs certain attributes, e.g. fulfill security standards, which are specified by the data owner.



[Data Provenance Tracking](data-provenance-tracking/README.md) deals with provision of information on how, when and from whom a data set was modified.
This security concepts helps with the clarification of grievances between a data owner and a data consumer, e.g. if a data consumer does not treat the data how he is supposed to.
The information on data modification is logged and saved at a so called Clearing House and can be viewed by the owner and consumer.

[Data Usage Control](data-usage-control/README.md) specifies, how a data set can be used after access is already granted.
Therefore, it is an extension of the data access control.
Actions that are permitted, prohibited and require other actions to be executed are specified in an usage contract that is accepted by the data owner and data consumer.


[Identity Management](identity-management/README.md) guarantees, that an entity you are interacting with really is the entity it claims to be.
In the International Data Space, this is handled with certificates that are handed out for a certain period of time and contain the most important static information.
These certificates are used by participants for authentication and encryption.

[Trusted Platform](trusted-platform/README.md) is a collection of local and distributed measures to ensure trust within the whole system.


[Trust Management](trust-management/README.md) handles the trust between communicating participants within the International Data Space.
Each connector has a security profile which represents a collection of security attributes.
Upon request, each connector has to present its security profile, so other participants can decide whether to trade data with one another.


The folder `integrated-examples-dac-duc-dpt` contains an integrated example of the three security concepts and checks,
- Data Access Control Check
- Data Usage Control Check
- Data Provenance Tracking Check  
, which can be applied to activity diagrams.

On the other hand the folder `integrated-examples-tm-tp-im` contains an integrated example of the three security concepts and checks,
- Trust Management Check
- Trusted Platform Check
- Identity Management Check  
, which can be applied to deployment diagrams.

The seperated examples for the different security concepts can be found in the other folders.
