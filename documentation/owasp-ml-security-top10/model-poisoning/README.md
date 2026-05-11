# Model Poisoning Check

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML10_2023-Model_Poisoning.html) for reference.

The check detects a possible Model Poisoning Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:
  
## 1. Regularisation
> Adding regularisation techniques like L1 or L2 regularization to the loss function helps to prevent overfitting and reduce the chance of model poisoning attacks.

Check condition: All Artifacts with stereotype `<<AI Algorithm>>` need to have `{Regularisation}` set to `true`.

## 2. Robust Model Design
> Designing models with robust architectures and activation functions can help reduce the chances of successful model poisoning attacks.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{RobustArchitecture}` and `{RobustActivationFunction}` set to `true`.

## 3. Cryptographic Techniques
> Cryptographic techniques can be used to secure the parameters and weights of the model, and prevent unauthorized access or manipulation of these parameters.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{CryptographicallySecured}` set to `true`.