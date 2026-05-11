# Output Integrity Attack Check

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML09_2023-Output_Integrity_Attack.html) for reference.

The check detects a possible Output Integrity Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

## 1. Using Cryptographic Methods 

> Cryptographic methods like digital signatures and secure hashes can be used to verify the authenticity of the results.

Check condition: All Artifacts with stereotype `<<AI Application>>` need to have `{CheckModelResultAuthenticity}` set to `true`.

## 2. Secure Communication Channels

> Communication channels between the model and the interface responsible for displaying the results should be secured using secure protocols such as SSL/TLS.

Check condition: All communication paths between a node with deployed artifact with stereotype `<<MLModel>>` and a node with deployed artifact with stereotype `<<AI Application>>` that depends on the `<<AI Model>>` need to have the stereotypes `<<integrity>>` and `<<secrecy>>`.

## 3. Input Validation

> Input validation should be performed on the results to check for unexpected or manipulated values.

Check condition: All artifacts with stereotype `<<AI Application>>` need to have `{InputValidation}` set to  `true`.

## 4. Tamper-evident Logs

> Maintaining tamper-evident logs of all input and output interactions can help detect and respond to any output integrity attacks.

Check condition: All artifacts with stereotype `<<AI Application>>` need to have `{TamperEvidentLogging}` set to `true`.

## 5. Regular Software Updates

> Regular software updates to fix vulnerabilities and security patches can help reduce the risk of output integrity attacks.

Check condition: The whole model needs to have the stereotype `<<SecureAIScenario>>` and needs to have `{RegularPackageUpdates}` set to `true`.

## 6. Monitoring and Auditing

> Regular monitoring and auditing of the results and the interactions between the model and the interface can help detect any suspicious activities and respond accordingly.

Check condition: All artifacts with `<<AI Application>>` and `<<ML Model>>` which are connected with a dependency need to have `{RegularAuditAndMonitoring}` set to `true`. 