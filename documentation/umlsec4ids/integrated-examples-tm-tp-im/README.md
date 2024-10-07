# Integrated Example for Trust Management, Trusted Platform & Identity Management

**Please inform yourself about** secure links, trust management, trusted platform and identity management before you study this example.
Examples and explanations can be found within the examples folder.


Here, we got an integrated example, where three Connectors want to communicate with on another.
Connector1 wants to communicate with Connector2 via an encrypted communication channel, which ensures integrity and secrecy.
They both got an security profile with `Trust` and `Trustplus`, whereas communication between them is allowed.
In addition to that, they all have sufficient certificates and security standards to be part of the secure ecosystem.

The same applies for the communication between Connector2 and Connector3, except that Connector3 has the `basefree` security profile instead of `trust`.
Therefore, only a directed communication is possible.