/**
 */
package mltop10;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see mltop10.Mltop10Package
 * @generated
 */
public interface Mltop10Factory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	Mltop10Factory eINSTANCE = mltop10.impl.Mltop10FactoryImpl.init();

	/**
	 * Returns a new object of class '<em>ML Model</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>ML Model</em>'.
	 * @generated
	 */
	MLModel createMLModel();

	/**
	 * Returns a new object of class '<em>AI Algorithm</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>AI Algorithm</em>'.
	 * @generated
	 */
	AIAlgorithm createAIAlgorithm();

	/**
	 * Returns a new object of class '<em>Training Data</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Training Data</em>'.
	 * @generated
	 */
	TrainingData createTrainingData();

	/**
	 * Returns a new object of class '<em>AI Application</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>AI Application</em>'.
	 * @generated
	 */
	AIApplication createAIApplication();

	/**
	 * Returns a new object of class '<em>Training Data Server</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Training Data Server</em>'.
	 * @generated
	 */
	TrainingDataServer createTrainingDataServer();

	/**
	 * Returns a new object of class '<em>Secure AI Scenario</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Secure AI Scenario</em>'.
	 * @generated
	 */
	SecureAIScenario createSecureAIScenario();

	/**
	 * Returns a new object of class '<em>Feedback Data</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Feedback Data</em>'.
	 * @generated
	 */
	FeedbackData createFeedbackData();

	/**
	 * Returns a new object of class '<em>integrity</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>integrity</em>'.
	 * @generated
	 */
	integrity createintegrity();

	/**
	 * Returns a new object of class '<em>secrecy</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>secrecy</em>'.
	 * @generated
	 */
	secrecy createsecrecy();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	Mltop10Package getMltop10Package();

} //Mltop10Factory
