/**
 */
package UMLsecenc.util;

import UMLsecenc.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see UMLsecenc.UMLsecencPackage
 * @generated
 */
public class UMLsecencAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static UMLsecencPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UMLsecencAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = UMLsecencPackage.eINSTANCE;
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
	protected UMLsecencSwitch<Adapter> modelSwitch =
		new UMLsecencSwitch<Adapter>() {
			@Override
			public Adapter casesecurelinksenc(securelinksenc object) {
				return createsecurelinksencAdapter();
			}
			@Override
			public Adapter casesecrecyenc(secrecyenc object) {
				return createsecrecyencAdapter();
			}
			@Override
			public Adapter caseencryptedenc(encryptedenc object) {
				return createencryptedencAdapter();
			}
			@Override
			public Adapter caseencryptedpersistence(encryptedpersistence object) {
				return createencryptedpersistenceAdapter();
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
	 * Creates a new adapter for an object of class '{@link UMLsecenc.securelinksenc <em>securelinksenc</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsecenc.securelinksenc
	 * @generated
	 */
	public Adapter createsecurelinksencAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsecenc.secrecyenc <em>secrecyenc</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsecenc.secrecyenc
	 * @generated
	 */
	public Adapter createsecrecyencAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsecenc.encryptedenc <em>encryptedenc</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsecenc.encryptedenc
	 * @generated
	 */
	public Adapter createencryptedencAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsecenc.encryptedpersistence <em>encryptedpersistence</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsecenc.encryptedpersistence
	 * @generated
	 */
	public Adapter createencryptedpersistenceAdapter() {
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

} //UMLsecencAdapterFactory
