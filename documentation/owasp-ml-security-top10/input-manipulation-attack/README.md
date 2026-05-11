# Input Manipulation Attack Check

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML01_2023-Input_Manipulation_Attack.html) for reference.

The check detects a possible Input Manipulation Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:
  
## 1. Adversarial training
> One approach to defending against input manipulation attack is to train the model on adversarial examples. This can help the model become more robust to attacks and reduce its susceptibility to being misled.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{AdversarialTraining}` set to `true`.

## 2. Robust models
> Another approach is to use models that are designed to be robust against manipulative attacks, such as adversarial training or models that incorporate defense mechanisms.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{AdversarialTraining}` and `{DefenseMechanism}` set to `true`.

## 3. Input validation 
> Input validation is another important defense mechanism that can be used to detect and prevent input manipulation attacks. This involves checking the input data for anomalies, such as unexpected values or patterns, and rejecting inputs that are likely to be malicious.

Check condition: All Artifacts with stereotype `<<AI Application>>` need to have `{InputValidation}` set to `true`.
