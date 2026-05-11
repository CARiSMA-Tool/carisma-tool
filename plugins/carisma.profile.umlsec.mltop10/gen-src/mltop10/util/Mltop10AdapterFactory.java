/**
 */
package mltop10.util;

import mltop10.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see mltop10.Mltop10Package
 * @generated
 */
public class Mltop10AdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static Mltop10Package modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Mltop10AdapterFactory() {
		if (modelPackage == null) {
			modelPackage = Mltop10Package.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected Mltop10Switch<Adapter> modelSwitch =
		new Mltop10Switch<Adapter>() {
			@Override
			public Adapter caseMLModel(MLModel object) {
				return createMLModelAdapter();
			}
			@Override
			public Adapter caseAIAlgorithm(AIAlgorithm object) {
				return createAIAlgorithmAdapter();
			}
			@Override
			public Adapter caseTrainingData(TrainingData object) {
				return createTrainingDataAdapter();
			}
			@Override
			public Adapter caseAIApplication(AIApplication object) {
				return createAIApplicationAdapter();
			}
			@Override
			public Adapter caseTrainingDataServer(TrainingDataServer object) {
				return createTrainingDataServerAdapter();
			}
			@Override
			public Adapter caseSecureAIScenario(SecureAIScenario object) {
				return createSecureAIScenarioAdapter();
			}
			@Override
			public Adapter caseFeedbackData(FeedbackData object) {
				return createFeedbackDataAdapter();
			}
			@Override
			public Adapter caseintegrity(integrity object) {
				return createintegrityAdapter();
			}
			@Override
			public Adapter casesecrecy(secrecy object) {
				return createsecrecyAdapter();
			}
			@Override
			public Adapter defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link mltop10.MLModel <em>ML Model</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.MLModel
	 * @generated
	 */
	public Adapter createMLModelAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.AIAlgorithm <em>AI Algorithm</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.AIAlgorithm
	 * @generated
	 */
	public Adapter createAIAlgorithmAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.TrainingData <em>Training Data</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.TrainingData
	 * @generated
	 */
	public Adapter createTrainingDataAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.AIApplication <em>AI Application</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.AIApplication
	 * @generated
	 */
	public Adapter createAIApplicationAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.TrainingDataServer <em>Training Data Server</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.TrainingDataServer
	 * @generated
	 */
	public Adapter createTrainingDataServerAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.SecureAIScenario <em>Secure AI Scenario</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.SecureAIScenario
	 * @generated
	 */
	public Adapter createSecureAIScenarioAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.FeedbackData <em>Feedback Data</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.FeedbackData
	 * @generated
	 */
	public Adapter createFeedbackDataAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.integrity <em>integrity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.integrity
	 * @generated
	 */
	public Adapter createintegrityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link mltop10.secrecy <em>secrecy</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see mltop10.secrecy
	 * @generated
	 */
	public Adapter createsecrecyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

} //Mltop10AdapterFactory
