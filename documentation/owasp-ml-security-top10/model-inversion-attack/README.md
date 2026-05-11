# Model Inversion Attack Check
  
See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML03_2023-Model_Inversion_Attack.html) for reference.

The check detects a possible Model Inversion Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

## Pre Check. Public model, training data and algorithm
> A model inversion attack can be carried out even without an explicit attack when the training data, the model, and the underlying algorithm are publicly available, as this transparency can enable the reconstruction of sensitive input information from the model’s outputs.

Check condition: A `{Public}` `<<ML Model>>`, `<<Training Data>>` and `<<AI Algorithm>>` always leads to a positive check.

## 1. Access control
> Limiting access to the model or its predictions can prevent attackers from obtaining the information needed to invert the model. This can be done by requiring authentication, encryption, or other forms of security when accessing the model or its predictions.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{AccessControl}` set to `true`. 

## 2. Input validation
> Validating the inputs to the model can prevent attackers from providing malicious data that can be used to invert the model. This can be done by checking the format, range, and consistency of the inputs before they are processed by the model.

Check condition: All artifacts with stereotype `<<AI Application>>` need to have `{InputValidation}` set to `true`.

## 3. Model transparency
> Making the model and its predictions transparent can help to detect and prevent model inversion attacks. This can be done by logging all inputs and outputs, providing explanations for the model’s predictions, or allowing users to inspect the model’s internal representations.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{Transparency}` set to `true`.

## 4. Regular monitoring
> Monitoring the model’s predictions for anomalies can help to detect and prevent model inversion attacks. This can be done by tracking the distribution of inputs and outputs, comparing the model’s predictions to ground truth data, or monitoring the model’s performance over time.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{AnomalyDetection}` set to `true`.

## 5. Model retraining
> Regularly retraining the model can help to prevent the information leaked by model inversion attacks from becoming outdated. This can be done by incorporating new data and correcting any inaccuracies in the model’s predictions.

Check condition: All artifacts with stereotype `<<ML Model>>` need to have `{RegularRetraining}` set to `true`.