# Data Poisoning Attack Check

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML02_2023-Data_Poisoning_Attack.html) for reference.

The check detects a possible Data Poisoning Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:
  
## 1. Data validation and verification
> Ensure that the training data is thoroughly validated and verified before it is used to train the model. This can be done by implementing data validation checks and employing multiple data labelers to validate the accuracy of the data labeling.

Check condition: All Artifacts with stereotype `<<Training Data>>` need to have `{Validation}` and `{Verification}` set to `true`.

## 2. Secure data storage
> Store the training data in a secure manner, such as using encryption, secure data transfer protocols, and firewalls.

Check condition: All Nodes with stereotype `<<Training Data Server>>` need to have `{SecureDataStorage}` set to `true`.

## 3. Data separation
> Separate the training data from the production data to reduce the risk of compromising the training data.

Check condition: All Artifacts must not have the stereotype `<<Training Data>>` and `<<ML Model>>` applied together.

## 4. Access control
> Implement access controls to limit who can access the training data and when they can access it.

Check condition: All Artifacts with stereotype `<<Training Data>>` need to have `{AccessControl}`set to `true`.

## 5. Monitoring and auditing:
> Regularly monitor the training data for any anomalies and conduct audits to detect any data tampering.

Check condition: All Artifacts with stereotype `<<Training Data>>` need to have `{RegularAuditAndMonitoring}`set to `true`.

## 6. Model validation
> Validate the model using a separate validation set that has not been used during training. This can help to detect any data poisoning attacks that may have affected the training data.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{Validation}`set to `true`.

## 7. Model ensembles
> Train multiple models using different subsets of the training data and use an ensemble of these models to make predictions. This can reduce the impact of data poisoning attacks as the attacker would need to compromise multiple models to achieve their goals.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{EnsembleModel}`set to `true`.

## 8. Anomaly detection
> Use anomaly detection techniques to detect any abnormal behavior in the training data, such as sudden changes in the data distribution or data labeling. These techniques can be used to detect data poisoning attacks early on.

Check condition: All Artifacts with stereotype `<<Training Data>>` need to have `{AnomalyDetection}`set to `true`.