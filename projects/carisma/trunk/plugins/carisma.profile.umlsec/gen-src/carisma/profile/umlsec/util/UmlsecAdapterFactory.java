/**
 */
package carisma.profile.umlsec.util;

import carisma.profile.umlsec.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.UmlsecPackage
 * @generated
 */
public class UmlsecAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static UmlsecPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UmlsecAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = UmlsecPackage.eINSTANCE;
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
	protected UmlsecSwitch<Adapter> modelSwitch =
		new UmlsecSwitch<Adapter>() {
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
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.protectedaction <em>protectedaction</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.protectedaction
	 * @generated
	 */
	public Adapter createprotectedactionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.rbac <em>rbac</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.rbac
	 * @generated
	 */
	public Adapter createrbacAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.SAPTransaction <em>SAP Transaction</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.SAPTransaction
	 * @generated
	 */
	public Adapter createSAPTransactionAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.allowedusers <em>allowedusers</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.allowedusers
	 * @generated
	 */
	public Adapter createallowedusersAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.seperationofduty <em>seperationofduty</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.seperationofduty
	 * @generated
	 */
	public Adapter createseperationofdutyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.usedby <em>usedby</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.usedby
	 * @generated
	 */
	public Adapter createusedbyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.identifiable <em>identifiable</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.identifiable
	 * @generated
	 */
	public Adapter createidentifiableAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.securedependency <em>securedependency</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.securedependency
	 * @generated
	 */
	public Adapter createsecuredependencyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.call <em>call</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.call
	 * @generated
	 */
	public Adapter createcallAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.send <em>send</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.send
	 * @generated
	 */
	public Adapter createsendAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.secrecy <em>secrecy</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.secrecy
	 * @generated
	 */
	public Adapter createsecrecyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.integrity <em>integrity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.integrity
	 * @generated
	 */
	public Adapter createintegrityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.high <em>high</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.high
	 * @generated
	 */
	public Adapter createhighAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.encrypted <em>encrypted</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.encrypted
	 * @generated
	 */
	public Adapter createencryptedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.wire <em>wire</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.wire
	 * @generated
	 */
	public Adapter createwireAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.Internet <em>Internet</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.Internet
	 * @generated
	 */
	public Adapter createInternetAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.LAN <em>LAN</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.LAN
	 * @generated
	 */
	public Adapter createLANAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.smartcard <em>smartcard</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.smartcard
	 * @generated
	 */
	public Adapter createsmartcardAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.POSdevice <em>PO Sdevice</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.POSdevice
	 * @generated
	 */
	public Adapter createPOSdeviceAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.issuernode <em>issuernode</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.issuernode
	 * @generated
	 */
	public Adapter createissuernodeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.critical <em>critical</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.critical
	 * @generated
	 */
	public Adapter createcriticalAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.datasecurity <em>datasecurity</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.datasecurity
	 * @generated
	 */
	public Adapter createdatasecurityAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.securelinks <em>securelinks</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.securelinks
	 * @generated
	 */
	public Adapter createsecurelinksAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.fairexchange <em>fairexchange</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.fairexchange
	 * @generated
	 */
	public Adapter createfairexchangeAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.provable <em>provable</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.provable
	 * @generated
	 */
	public Adapter createprovableAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.nodownflow <em>nodownflow</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.nodownflow
	 * @generated
	 */
	public Adapter createnodownflowAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.noupflow <em>noupflow</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.noupflow
	 * @generated
	 */
	public Adapter createnoupflowAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.guardedaccess <em>guardedaccess</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.guardedaccess
	 * @generated
	 */
	public Adapter createguardedaccessAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.guarded <em>guarded</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.guarded
	 * @generated
	 */
	public Adapter createguardedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.authorizedstatus <em>authorizedstatus</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.authorizedstatus
	 * @generated
	 */
	public Adapter createauthorizedstatusAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.lockedstatus <em>lockedstatus</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.lockedstatus
	 * @generated
	 */
	public Adapter createlockedstatusAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.requires <em>requires</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.requires
	 * @generated
	 */
	public Adapter createrequiresAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link carisma.profile.umlsec.privacy <em>privacy</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see carisma.profile.umlsec.privacy
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

} //UmlsecAdapterFactory
