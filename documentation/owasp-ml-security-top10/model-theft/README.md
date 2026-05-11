# Model Theft Check
  
See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML05_2023-Model_Theft.html) for reference.

The check detects a possible Model Theft Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

## Pre Check. Public model, training data and algorithm
> A model theft can be carried out even without an explicit attack when the training data, the model, and the underlying algorithm are publicly available, as this level of transparency effectively enables replication of the model’s functionality without requiring additional extraction techniques.

Check condition: All artifacts with stereotype `<<ML Model>>`, `<<AI Algorithm>>` and `<<TrainingData>>` need to have `{Public}` set to  `false`. 

## 1. Encryption
> Encrypting the model’s code, training data, and other sensitive information can prevent attackers from being able to access and steal the model.

Check condition: All Dependencies between an artifact with stereotype `<<ML Model>>` and `<<AI Algorithm>>` need to have `<<secrecy>>` between them. 

## 2. Access Control
> Implementing strict access control measures, such as two-factor authentication, can prevent unauthorized individuals from accessing and stealing the model.

Check condition: All artifacts with stereotype `<<ML Model>>`, `<<AI Algorithm>>` and `<<TrainingData>>` need to have `{AccessControl}` set to  `true`.

## 3. Regular backups
> Regularly backing up the model’s code, training data, and other sensitive information can ensure that it can be recovered in the event of a theft.

Check condition: All artifacts with stereotype `<<ML Model>>` and `<<TrainingData>>` need to have `{RegularBackup}` set to  `true`.

## 4. Model Obfuscation
> Obfuscating the model’s code and making it difficult to reverse engineer can prevent attackers from being able to steal the model.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{Obfuscation}` set to  `true`.

## 5. Watermarking
> Adding a watermark to the model’s code and training data can make it possible to trace the source of a theft and hold the attacker accountable.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{Watermarking}` set to  `true`.

## 6. Legal protection
> Securing legal protection for the model, such as patents or trade secrets, can make it more difficult for an attacker to steal the model and can provide a basis for legal action in the event of a theft.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{LegalProtection}` set to  `true`.

## 7. Monitoring and auditing
> Regularly monitoring and auditing the model’s use can help detect and prevent theft by detecting when an attacker is attempting to access or steal the model.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{RegularAuditAndMonitoring}` set to  `true`.