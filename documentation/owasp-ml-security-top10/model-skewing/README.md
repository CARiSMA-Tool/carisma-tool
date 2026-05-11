# Model Skewing Check

See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML08_2023-Model_Skewing.html) for reference.

The check detects a possible Model Skewing Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

## Pre Check. No feedback data available
> A model skewing attack is not feasible when no feedback data is available, as the absence of a feedback channel prevents an adversary from influencing the model's behavior or iteratively biasing its outputs.

Check condition: Model Skewing is not possible, if no `<<FeedbackData>>` is available.

## 1. Implement robust access controls
> Ensure that only authorized personnel have access to the MLOps system and its feedback loops, and that all activities are logged and audited.

Check condition: All Artifacts with stereotypes `<<ML Model>>` and `<<FeedbackData>>` need to have `{AccessControl}` set to `true`. 

## 2. Verify the authenticity of feedback data
> Use techniques such as digital signatures and checksums to verify that the feedback data received by the system is genuine, and reject any data that does not match the expected format.

Check condition: All Artifacts with stereotype `<<FeedbackData>>` need to have `{AuthenticityVerified}` set to `true`.

## 3. Use data validation and cleaning techniques
> Clean and validate the feedback data before using it to update the training data, to minimize the risk of incorrect or malicious data being used.

Check condition: All Artifacts with stereotype `<<FeedbackData>>` need to have `{Validation}` and `{Cleaning}` set to `true`.

## 4. Implement anomaly detection
> Use techniques such as statistical and machine learning-based methods to detect and alert on anomalies in the feedback data, which could indicate an attack.

Check condition: All Artifacts with stereotype `<<FeedbackData>>` need to have `{AnomalyDetection}` set to `true`.

## 5. Regularly monitor the model’s performance
> Continuously monitor the performance of the model, and compare its predictions with actual outcomes to detect any deviation or skewing.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{RegularPerformanceMonitoring}` set to `true`.

## 6. Continuously train the model
> Regularly retrain the model using updated and verified training data, to ensure that it continues to reflect the latest information and trends.

Check condition: All Artifacts with stereotype `<<ML Model>>` need to have `{RegularRetraining}` set to `true`.