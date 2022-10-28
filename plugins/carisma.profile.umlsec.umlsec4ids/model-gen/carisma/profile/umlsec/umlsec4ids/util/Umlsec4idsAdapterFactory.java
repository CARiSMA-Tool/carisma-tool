/**
 */
package carisma.profile.umlsec.umlsec4ids.util;

import carisma.profile.umlsec.umlsec4ids.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage
 * @generated
 */
public class Umlsec4idsAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static Umlsec4idsPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Umlsec4idsAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = Umlsec4idsPackage.eINSTANCE;
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
	protected Umlsec4idsSwitch<Adapter> modelSwitch =
		new Umlsec4idsSwitch<Adapter>() {
			@Override
			public Adapter casebasefree(basefree object) {
				return createbasefreeAdapter();
			}
			@Override
			public Adapter casebase(base object) {
				return createbaseAdapter();
			}
			@Override
			public Adapter casetrust(trust object) {
				return createtrustAdapter();
			}
			@Override
			public Adapter casetrustplus(trustplus object) {
				return createtrustplusAdapter();
			}
			@Override
			public Adapter casedataprovenancetracking(dataprovenancetracking object) {
				return createdataprovenancetrackingAdapter();
			}
			@Override
			public Adapter caseOwner(Owner object) {
				return createOwnerAdapter();
			}
			@Override
			public Adapter caseConsumer(Consumer object) {
				return createConsumerAdapter();
			}
			@Override
			public Adapter caseX509(X509 object) {
				return createX509Adapter();
			}
			@Override
			public Adapter caseX509TLS(X509TLS object) {
				return createX509TLSAdapter();
			}
			@Override
			public Adapter casedatausagecontrol(datausagecontrol object) {
				return createdatausagecontrolAdapter();
			}
			@Override
			public Adapter caseverified(verified object) {
				return createverifiedAdapter();
			}
			@Override
			public Adapter caseisolated(isolated object) {
				return createisolatedAdapter();
			}
			@Override
			public Adapter caseencryption(encryption object) {
				return createencryptionAdapter();
			}
			@Override
			public Adapter casecertified(certified object) {
				return createcertifiedAdapter();
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
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.basefree <em>basefree</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.basefree
	 * @generated
	 */
	public Adapter createbasefreeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.base <em>base</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.base
	 * @generated
	 */
	public Adapter createbaseAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.trust <em>trust</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.trust
	 * @generated
	 */
	public Adapter createtrustAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.trustplus <em>trustplus</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.trustplus
	 * @generated
	 */
	public Adapter createtrustplusAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking <em>dataprovenancetracking</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.dataprovenancetracking
	 * @generated
	 */
	public Adapter createdataprovenancetrackingAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.Owner <em>Owner</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.Owner
	 * @generated
	 */
	public Adapter createOwnerAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.Consumer <em>Consumer</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.Consumer
	 * @generated
	 */
	public Adapter createConsumerAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.X509 <em>X509</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.X509
	 * @generated
	 */
	public Adapter createX509Adapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.X509TLS <em>X509TLS</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.X509TLS
	 * @generated
	 */
	public Adapter createX509TLSAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol <em>datausagecontrol</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.datausagecontrol
	 * @generated
	 */
	public Adapter createdatausagecontrolAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.verified <em>verified</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.verified
	 * @generated
	 */
	public Adapter createverifiedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.isolated <em>isolated</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.isolated
	 * @generated
	 */
	public Adapter createisolatedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.encryption <em>encryption</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.encryption
	 * @generated
	 */
	public Adapter createencryptionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.umlsec4ids.certified <em>certified</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.umlsec4ids.certified
	 * @generated
	 */
	public Adapter createcertifiedAdapter() {
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

} //Umlsec4idsAdapterFactory
