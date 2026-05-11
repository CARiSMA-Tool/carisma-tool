# Transfer Learning Attack Check

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML07_2023-Transfer_Learning_Attack.html) for reference.

The check detects a possible Transfer Learning Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

## 1. Regularly monitor and update the training datasets
> Regularly monitoring and updating the training datasets can help prevent the transfer of malicious knowledge from the attacker's model to the target model.

Check condition: All Artifacts with stereotype `<<TrainingData>>` need to have `{RegularUpdatesAndTraining}` set to `true`.

## 2. Use secure and trusted training datasets
> Using secure and trusted training datasets can help prevent the transfer of malicious knowledge from the attacker’s model to the target model.

Check condition: All Artifacts with stereotype `<<TrainingData>>` need to have `{Trusted}` set to `true`.

## 3. Implement model isolation
>Implementing model isolation can help prevent the transfer of malicious knowledge from one model to another. For example, separating the training and deployment environments can prevent attackers from transferring knowledge from the training environment to the deployment environment.

Check condition: All `<<ML Model>>`, `<<AIAlgorithm>>` and `<<TrainingData>>` must not be deployed on the same  Artifact.

## 4. Use differential privacy
> Using differential privacy can help protect the privacy of individual records in the training dataset and prevent the transfer of malicious knowledge from the attacker’s model to the target model.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{DifferentialPrivacy}` set to `true`.

## 5. Perform regular security audits
> Regular security audits can help identify and prevent transfer learning attacks by identifying and addressing vulnerabilities in the system.

Check condition: All Artifacts with stereotype `<<SecureAIScenario>>` need to have `{RegularSecurityAudits}` set to `true`.