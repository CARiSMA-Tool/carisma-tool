/**
 */
package UMLsec.util;

import UMLsec.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see UMLsec.UMLsecPackage
 * @generated
 */
public class UMLsecAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static UMLsecPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UMLsecAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = UMLsecPackage.eINSTANCE;
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
	protected UMLsecSwitch<Adapter> modelSwitch =
		new UMLsecSwitch<Adapter>() {
			@Override
			public Adapter caseprotectedaction(protectedaction object) {
				return createprotectedactionAdapter();
			}
			@Override
			public Adapter caserbac(rbac object) {
				return createrbacAdapter();
			}
			@Override
			public Adapter caseSAPTransaction(SAPTransaction object) {
				return createSAPTransactionAdapter();
			}
			@Override
			public Adapter caseallowedusers(allowedusers object) {
				return createallowedusersAdapter();
			}
			@Override
			public Adapter caseseperationofduty(seperationofduty object) {
				return createseperationofdutyAdapter();
			}
			@Override
			public Adapter caseusedby(usedby object) {
				return createusedbyAdapter();
			}
			@Override
			public Adapter caseidentifiable(identifiable object) {
				return createidentifiableAdapter();
			}
			@Override
			public Adapter casesecuredependency(securedependency object) {
				return createsecuredependencyAdapter();
			}
			@Override
			public Adapter casecall(call object) {
				return createcallAdapter();
			}
			@Override
			public Adapter casesend(send object) {
				return createsendAdapter();
			}
			@Override
			public Adapter casesecrecy(secrecy object) {
				return createsecrecyAdapter();
			}
			@Override
			public Adapter caseintegrity(integrity object) {
				return createintegrityAdapter();
			}
			@Override
			public Adapter casehigh(high object) {
				return createhighAdapter();
			}
			@Override
			public Adapter caseencrypted(encrypted object) {
				return createencryptedAdapter();
			}
			@Override
			public Adapter casewire(wire object) {
				return createwireAdapter();
			}
			@Override
			public Adapter caseInternet(Internet object) {
				return createInternetAdapter();
			}
			@Override
			public Adapter caseLAN(LAN object) {
				return createLANAdapter();
			}
			@Override
			public Adapter casesmartcard(smartcard object) {
				return createsmartcardAdapter();
			}
			@Override
			public Adapter casePOSdevice(POSdevice object) {
				return createPOSdeviceAdapter();
			}
			@Override
			public Adapter caseissuernode(issuernode object) {
				return createissuernodeAdapter();
			}
			@Override
			public Adapter casecritical(critical object) {
				return createcriticalAdapter();
			}
			@Override
			public Adapter casedatasecurity(datasecurity object) {
				return createdatasecurityAdapter();
			}
			@Override
			public Adapter casesecurelinks(securelinks object) {
				return createsecurelinksAdapter();
			}
			@Override
			public Adapter casefairexchange(fairexchange object) {
				return createfairexchangeAdapter();
			}
			@Override
			public Adapter caseprovable(provable object) {
				return createprovableAdapter();
			}
			@Override
			public Adapter casenodownflow(nodownflow object) {
				return createnodownflowAdapter();
			}
			@Override
			public Adapter casenoupflow(noupflow object) {
				return createnoupflowAdapter();
			}
			@Override
			public Adapter caseguardedaccess(guardedaccess object) {
				return createguardedaccessAdapter();
			}
			@Override
			public Adapter caseguarded(guarded object) {
				return createguardedAdapter();
			}
			@Override
			public Adapter caseauthorizedstatus(authorizedstatus object) {
				return createauthorizedstatusAdapter();
			}
			@Override
			public Adapter caselockedstatus(lockedstatus object) {
				return createlockedstatusAdapter();
			}
			@Override
			public Adapter caserequires(requires object) {
				return createrequiresAdapter();
			}
			@Override
			public Adapter caseprivacy(privacy object) {
				return createprivacyAdapter();
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
	 * Creates a new adapter for an object of class '{@link UMLsec.protectedaction <em>protectedaction</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.protectedaction
	 * @generated
	 */
	public Adapter createprotectedactionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.rbac <em>rbac</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.rbac
	 * @generated
	 */
	public Adapter createrbacAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.SAPTransaction <em>SAP Transaction</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.SAPTransaction
	 * @generated
	 */
	public Adapter createSAPTransactionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.allowedusers <em>allowedusers</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.allowedusers
	 * @generated
	 */
	public Adapter createallowedusersAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.seperationofduty <em>seperationofduty</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.seperationofduty
	 * @generated
	 */
	public Adapter createseperationofdutyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.usedby <em>usedby</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.usedby
	 * @generated
	 */
	public Adapter createusedbyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.identifiable <em>identifiable</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.identifiable
	 * @generated
	 */
	public Adapter createidentifiableAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.securedependency <em>securedependency</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.securedependency
	 * @generated
	 */
	public Adapter createsecuredependencyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.call <em>call</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.call
	 * @generated
	 */
	public Adapter createcallAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.send <em>send</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.send
	 * @generated
	 */
	public Adapter createsendAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.secrecy <em>secrecy</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.secrecy
	 * @generated
	 */
	public Adapter createsecrecyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.integrity <em>integrity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.integrity
	 * @generated
	 */
	public Adapter createintegrityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.high <em>high</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.high
	 * @generated
	 */
	public Adapter createhighAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.encrypted <em>encrypted</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.encrypted
	 * @generated
	 */
	public Adapter createencryptedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.wire <em>wire</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.wire
	 * @generated
	 */
	public Adapter createwireAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.Internet <em>Internet</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.Internet
	 * @generated
	 */
	public Adapter createInternetAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.LAN <em>LAN</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.LAN
	 * @generated
	 */
	public Adapter createLANAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.smartcard <em>smartcard</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.smartcard
	 * @generated
	 */
	public Adapter createsmartcardAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.POSdevice <em>PO Sdevice</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.POSdevice
	 * @generated
	 */
	public Adapter createPOSdeviceAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.issuernode <em>issuernode</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.issuernode
	 * @generated
	 */
	public Adapter createissuernodeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.critical <em>critical</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.critical
	 * @generated
	 */
	public Adapter createcriticalAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.datasecurity <em>datasecurity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.datasecurity
	 * @generated
	 */
	public Adapter createdatasecurityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.securelinks <em>securelinks</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.securelinks
	 * @generated
	 */
	public Adapter createsecurelinksAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.fairexchange <em>fairexchange</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.fairexchange
	 * @generated
	 */
	public Adapter createfairexchangeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.provable <em>provable</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.provable
	 * @generated
	 */
	public Adapter createprovableAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.nodownflow <em>nodownflow</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.nodownflow
	 * @generated
	 */
	public Adapter createnodownflowAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.noupflow <em>noupflow</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.noupflow
	 * @generated
	 */
	public Adapter createnoupflowAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.guardedaccess <em>guardedaccess</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.guardedaccess
	 * @generated
	 */
	public Adapter createguardedaccessAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.guarded <em>guarded</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.guarded
	 * @generated
	 */
	public Adapter createguardedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.authorizedstatus <em>authorizedstatus</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.authorizedstatus
	 * @generated
	 */
	public Adapter createauthorizedstatusAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.lockedstatus <em>lockedstatus</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.lockedstatus
	 * @generated
	 */
	public Adapter createlockedstatusAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.requires <em>requires</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.requires
	 * @generated
	 */
	public Adapter createrequiresAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link UMLsec.privacy <em>privacy</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see UMLsec.privacy
	 * @generated
	 */
	public Adapter createprivacyAdapter() {
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

} //UMLsecAdapterFactory
