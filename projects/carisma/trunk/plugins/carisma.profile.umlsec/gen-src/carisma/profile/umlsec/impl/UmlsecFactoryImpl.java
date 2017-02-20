/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.*;

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
public class UmlsecFactoryImpl extends EFactoryImpl implements UmlsecFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static UmlsecFactory init() {
		try {
			UmlsecFactory theUmlsecFactory = (UmlsecFactory)EPackage.Registry.INSTANCE.getEFactory(UmlsecPackage.eNS_URI);
			if (theUmlsecFactory != null) {
				return theUmlsecFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new UmlsecFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UmlsecFactoryImpl() {
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
			case UmlsecPackage.PROTECTEDACTION: return createprotectedaction();
			case UmlsecPackage.RBAC: return createrbac();
			case UmlsecPackage.SAP_TRANSACTION: return createSAPTransaction();
			case UmlsecPackage.ALLOWEDUSERS: return createallowedusers();
			case UmlsecPackage.SEPERATIONOFDUTY: return createseperationofduty();
			case UmlsecPackage.USEDBY: return createusedby();
			case UmlsecPackage.IDENTIFIABLE: return createidentifiable();
			case UmlsecPackage.SECUREDEPENDENCY: return createsecuredependency();
			case UmlsecPackage.CALL: return createcall();
			case UmlsecPackage.SEND: return createsend();
			case UmlsecPackage.SECRECY: return createsecrecy();
			case UmlsecPackage.INTEGRITY: return createintegrity();
			case UmlsecPackage.HIGH: return createhigh();
			case UmlsecPackage.ENCRYPTED: return createencrypted();
			case UmlsecPackage.WIRE: return createwire();
			case UmlsecPackage.INTERNET: return createInternet();
			case UmlsecPackage.LAN: return createLAN();
			case UmlsecPackage.SMARTCARD: return createsmartcard();
			case UmlsecPackage.PO_SDEVICE: return createPOSdevice();
			case UmlsecPackage.ISSUERNODE: return createissuernode();
			case UmlsecPackage.CRITICAL: return createcritical();
			case UmlsecPackage.DATASECURITY: return createdatasecurity();
			case UmlsecPackage.SECURELINKS: return createsecurelinks();
			case UmlsecPackage.FAIREXCHANGE: return createfairexchange();
			case UmlsecPackage.PROVABLE: return createprovable();
			case UmlsecPackage.NODOWNFLOW: return createnodownflow();
			case UmlsecPackage.NOUPFLOW: return createnoupflow();
			case UmlsecPackage.GUARDEDACCESS: return createguardedaccess();
			case UmlsecPackage.GUARDED: return createguarded();
			case UmlsecPackage.AUTHORIZEDSTATUS: return createauthorizedstatus();
			case UmlsecPackage.LOCKEDSTATUS: return createlockedstatus();
			case UmlsecPackage.REQUIRES: return createrequires();
			case UmlsecPackage.PRIVACY: return createprivacy();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public protectedaction createprotectedaction() {
		protectedactionImpl protectedaction = new protectedactionImpl();
		return protectedaction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public rbac createrbac() {
		rbacImpl rbac = new rbacImpl();
		return rbac;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SAPTransaction createSAPTransaction() {
		SAPTransactionImpl sapTransaction = new SAPTransactionImpl();
		return sapTransaction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public allowedusers createallowedusers() {
		allowedusersImpl allowedusers = new allowedusersImpl();
		return allowedusers;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public seperationofduty createseperationofduty() {
		seperationofdutyImpl seperationofduty = new seperationofdutyImpl();
		return seperationofduty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public usedby createusedby() {
		usedbyImpl usedby = new usedbyImpl();
		return usedby;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public identifiable createidentifiable() {
		identifiableImpl identifiable = new identifiableImpl();
		return identifiable;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public securedependency createsecuredependency() {
		securedependencyImpl securedependency = new securedependencyImpl();
		return securedependency;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public call createcall() {
		callImpl call = new callImpl();
		return call;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public send createsend() {
		sendImpl send = new sendImpl();
		return send;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public secrecy createsecrecy() {
		secrecyImpl secrecy = new secrecyImpl();
		return secrecy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public integrity createintegrity() {
		integrityImpl integrity = new integrityImpl();
		return integrity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public high createhigh() {
		highImpl high = new highImpl();
		return high;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public encrypted createencrypted() {
		encryptedImpl encrypted = new encryptedImpl();
		return encrypted;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public wire createwire() {
		wireImpl wire = new wireImpl();
		return wire;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Internet createInternet() {
		InternetImpl internet = new InternetImpl();
		return internet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LAN createLAN() {
		LANImpl lan = new LANImpl();
		return lan;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public smartcard createsmartcard() {
		smartcardImpl smartcard = new smartcardImpl();
		return smartcard;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public POSdevice createPOSdevice() {
		POSdeviceImpl poSdevice = new POSdeviceImpl();
		return poSdevice;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public issuernode createissuernode() {
		issuernodeImpl issuernode = new issuernodeImpl();
		return issuernode;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public critical createcritical() {
		criticalImpl critical = new criticalImpl();
		return critical;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public datasecurity createdatasecurity() {
		datasecurityImpl datasecurity = new datasecurityImpl();
		return datasecurity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public securelinks createsecurelinks() {
		securelinksImpl securelinks = new securelinksImpl();
		return securelinks;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public fairexchange createfairexchange() {
		fairexchangeImpl fairexchange = new fairexchangeImpl();
		return fairexchange;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public provable createprovable() {
		provableImpl provable = new provableImpl();
		return provable;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public nodownflow createnodownflow() {
		nodownflowImpl nodownflow = new nodownflowImpl();
		return nodownflow;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public noupflow createnoupflow() {
		noupflowImpl noupflow = new noupflowImpl();
		return noupflow;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public guardedaccess createguardedaccess() {
		guardedaccessImpl guardedaccess = new guardedaccessImpl();
		return guardedaccess;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public guarded createguarded() {
		guardedImpl guarded = new guardedImpl();
		return guarded;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public authorizedstatus createauthorizedstatus() {
		authorizedstatusImpl authorizedstatus = new authorizedstatusImpl();
		return authorizedstatus;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public lockedstatus createlockedstatus() {
		lockedstatusImpl lockedstatus = new lockedstatusImpl();
		return lockedstatus;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public requires createrequires() {
		requiresImpl requires = new requiresImpl();
		return requires;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public privacy createprivacy() {
		privacyImpl privacy = new privacyImpl();
		return privacy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UmlsecPackage getUmlsecPackage() {
		return (UmlsecPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static UmlsecPackage getPackage() {
		return UmlsecPackage.eINSTANCE;
	}

} //UmlsecFactoryImpl
