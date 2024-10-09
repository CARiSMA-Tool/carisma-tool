# Integrated Example for Data Access Control, Data Usage Control & Data Provenance Tracking

**Please inform yourself about** data access control, data usage control and data provenance tracking before you study this example.
Examples and explanations can be found within the examples folder.

Here, we can see three actors which is a consumer, an owner and a clearing house.
The owner and consumer are annotated with the stereotypes and the consumer has sufficients attributes and actions he want to execute and therefore gets access when the protected action "Check Consumers Attributes" is executed.
After the consumer accepted a usage contract, he got an annotated partitione with the `datausagecontrol` stereotype.
There he is permitted every action he wants to execute and has as obligation the action "Send Copy To Owner", when the action "Write Into Data Set" is executed.
At last, the clearing house needs to execute "Log Performed Read Action On Data Set" after "Read Data Set" is performed.
