/**
 */
package mltop10.impl;

import mltop10.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class Mltop10FactoryImpl extends EFactoryImpl implements Mltop10Factory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static Mltop10Factory init() {
		try {
			Mltop10Factory theMltop10Factory = (Mltop10Factory)EPackage.Registry.INSTANCE.getEFactory(Mltop10Package.eNS_URI);
			if (theMltop10Factory != null) {
				return theMltop10Factory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new Mltop10FactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Mltop10FactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case Mltop10Package.ML_MODEL: return createMLModel();
			case Mltop10Package.AI_ALGORITHM: return createAIAlgorithm();
			case Mltop10Package.TRAINING_DATA: return createTrainingData();
			case Mltop10Package.AI_APPLICATION: return createAIApplication();
			case Mltop10Package.TRAINING_DATA_SERVER: return createTrainingDataServer();
			case Mltop10Package.SECURE_AI_SCENARIO: return createSecureAIScenario();
			case Mltop10Package.FEEDBACK_DATA: return createFeedbackData();
			case Mltop10Package.SECURE_COMM_PATH: return createSecureCommPath();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public MLModel createMLModel() {
		MLModelImpl mlModel = new MLModelImpl();
		return mlModel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public AIAlgorithm createAIAlgorithm() {
		AIAlgorithmImpl aiAlgorithm = new AIAlgorithmImpl();
		return aiAlgorithm;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public TrainingData createTrainingData() {
		TrainingDataImpl trainingData = new TrainingDataImpl();
		return trainingData;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public AIApplication createAIApplication() {
		AIApplicationImpl aiApplication = new AIApplicationImpl();
		return aiApplication;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public TrainingDataServer createTrainingDataServer() {
		TrainingDataServerImpl trainingDataServer = new TrainingDataServerImpl();
		return trainingDataServer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public SecureAIScenario createSecureAIScenario() {
		SecureAIScenarioImpl secureAIScenario = new SecureAIScenarioImpl();
		return secureAIScenario;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public FeedbackData createFeedbackData() {
		FeedbackDataImpl feedbackData = new FeedbackDataImpl();
		return feedbackData;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public SecureCommPath createSecureCommPath() {
		SecureCommPathImpl secureCommPath = new SecureCommPathImpl();
		return secureCommPath;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Mltop10Package getMltop10Package() {
		return (Mltop10Package)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static Mltop10Package getPackage() {
		return Mltop10Package.eINSTANCE;
	}

} //Mltop10FactoryImpl
