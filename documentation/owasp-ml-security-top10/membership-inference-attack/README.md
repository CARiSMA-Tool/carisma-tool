# Membership Inference Attack Check

This check detects a possible Membership Inference Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML04_2023-Membership_Inference_Attack.html) for further reference.

## Pre Check. Public training data and secure communication channels
Membership inference is possible without an attack when the training data is publicly available or communicated through a channel that does not ensure confidentiality.

- Check condition a: If a `<<Training Data>>` is defined as `Public`, this check stops (with a positive result containg a warning), since a attack is not necessary to infer membership. 
- Check condition b: Communication paths between two nodes with an artifact with stereotype `<<Training Data>>` deployed to one node and another artifact connected via a dependency with the `<<Training Data>>` deployed to the other node need to be have the stereotype `<<secrecy>>`. 

## 1. Model training on randomized or shuffled data
> "Training machine learning models on randomized or shuffled data can make it more difficult for an attacker to determine whether a particular example was included in the training dataset." -- *[OWASP](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML04_2023-Membership_Inference_Attack.html)*

- Check condition: All artifacts with stereotype `<<AI Algorithm>>` need to have `Randomize` set to `true`.  


## 2. Model Obfuscation
> "Obfuscating the model’s predictions by adding random noise or using differential privacy techniques can help prevent membership inference attacks by making it harder for an attacker to determine the model’s training data." -- *[OWASP](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML04_2023-Membership_Inference_Attack.html)*

- Check condition: All artifacts with stereotype `<<ML Model>>` need to have `Obfuscation` set to  `true`.

## 3. Regularisation
> "Regularisation techniques such as L1 or L2 regularization can help prevent overfitting of the model to the training data, which can reduce the model’s ability to accurately determine whether a particular example was included in the training dataset." -- *[OWASP](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML04_2023-Membership_Inference_Attack.html)*

- Check condition: All artifacts with stereotype `<<AI Algorithm>>` need to have `Regularisation` set to  `true`.

## 4. Reducing the training data
> "Reducing the size of the training dataset or removing redundant or highly correlated features can help reduce the information an attacker can gain from a membership inference attack." -- *[OWASP](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML04_2023-Membership_Inference_Attack.html)*

- Check condition: All artifacts with stereotype `<<Training Data>>` need to have `Reduced` set to  `true`.

## 5. Testing and monitoring
> "Regularly testing and monitoring the model’s behavior for anomalies can help detect and prevent membership inference attacks by detecting when an attacker is attempting to gain access to sensitive information." -- *[OWASP](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML04_2023-Membership_Inference_Attack.html)*

- Check condition: All artifacts with stereotype `<<ML Model>>` need to have `RegularTestingAndMonitoring` set to  `true`.
