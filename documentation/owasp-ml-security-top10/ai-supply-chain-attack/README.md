# AI Supply Chain Attacks Check
    
See [OWASP description of this attack](https://owasp.org/www-project-machine-learning-security-top-10/docs/ML09_2023-Output_Integrity_Attack.html) for reference.

The check detects a possible Output Integrity Attack, if at least one of the following prevention mechanisms as defined by OWASP is not recognized in the analyzed model:

## 1. Verify packages integrity
> Before using any packages in your infrastructure or application dependencies, verify the authenticity of the package by checking the digital signature of the package.

Check condition: All models/packages with stereotype `<<SecureAIScenario>>` need to have `{PackageIntegrityVerified}` set to  `true`.

## 2. Keep packages versions up-to-date
> Constantly monitor the latest versions of the packages in your software supply chain and update your dependencies if you are using outdated software. Use tools such as OWASP Dependency Check. Refer to https://owasp.org/Top10/A06_2021-Vulnerable_and_Outdated_Components/ for more details

Check condition: All models/packages with stereotype `<<SecureAIScenario>>` need to have `{RegularPackageUpdates}` set to  `true`.

## 3. Install packages from secure sources
> Use secure third-party software repositiories, such as Anaconda or pip, that enforce strict security measures and have a vetting process for packages.

Check condition: All models/packages with stereotype `<<SecureAIScenario>>` need to have `{PackagesFromSecureSources}` set to  `true`.

## 4. Deploy ML infrastructure securely
> Follow the vendor’s deployment recommendations for MLOps platforms in your stack, limit the access to the web UIs from the Internet, monitor the traffic in the infrastructure for the anomalies and possible attacks. If the infrastructure is deployed in the cloud, ensure to leverage the cloud provider’s security features such as Virtual Private Clouds (VPCs), security groups, and identity and access management (IAM) roles to restrict and control access. Implement strict access control measures. Ensure that only authorized personnel have access to the MLOps platforms.

Check condition: All models/packages with stereotype `<<SecureAIScenario>>` need to have `{SecureDeployment}` set to  `true`.