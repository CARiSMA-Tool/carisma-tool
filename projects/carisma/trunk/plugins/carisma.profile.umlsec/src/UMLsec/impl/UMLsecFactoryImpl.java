/**
 */
package UMLsec.impl;

import UMLsec.*;

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
public class UMLsecFactoryImpl extends EFactoryImpl implements UMLsecFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static UMLsecFactory init() {
		try {
			UMLsecFactory theUMLsecFactory = (UMLsecFactory)EPackage.Registry.INSTANCE.getEFactory(UMLsecPackage.eNS_URI);
			if (theUMLsecFactory != null) {
				return theUMLsecFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new UMLsecFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UMLsecFactoryImpl() {
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
			case UMLsecPackage.PROTECTEDACTION: return createprotectedaction();
			case UMLsecPackage.RBAC: return createrbac();
			case UMLsecPackage.SAP_TRANSACTION: return createSAPTransaction();
			case UMLsecPackage.ALLOWEDUSERS: return createallowedusers();
			case UMLsecPackage.SEPERATIONOFDUTY: return createseperationofduty();
			case UMLsecPackage.USEDBY: return createusedby();
			case UMLsecPackage.IDENTIFIABLE: return createidentifiable();
			case UMLsecPackage.SECUREDEPENDENCY: return createsecuredependency();
			case UMLsecPackage.CALL: return createcall();
			case UMLsecPackage.SEND: return createsend();
			case UMLsecPackage.SECRECY: return createsecrecy();
			case UMLsecPackage.INTEGRITY: return createintegrity();
			case UMLsecPackage.HIGH: return createhigh();
			case UMLsecPackage.ENCRYPTED: return createencrypted();
			case UMLsecPackage.WIRE: return createwire();
			case UMLsecPackage.INTERNET: return createInternet();
			case UMLsecPackage.LAN: return createLAN();
			case UMLsecPackage.SMARTCARD: return createsmartcard();
			case UMLsecPackage.PO_SDEVICE: return createPOSdevice();
			case UMLsecPackage.ISSUERNODE: return createissuernode();
			case UMLsecPackage.CRITICAL: return createcritical();
			case UMLsecPackage.DATASECURITY: return createdatasecurity();
			case UMLsecPackage.SECURELINKS: return createsecurelinks();
			case UMLsecPackage.FAIREXCHANGE: return createfairexchange();
			case UMLsecPackage.PROVABLE: return createprovable();
			case UMLsecPackage.NODOWNFLOW: return createnodownflow();
			case UMLsecPackage.NOUPFLOW: return createnoupflow();
			case UMLsecPackage.GUARDEDACCESS: return createguardedaccess();
			case UMLsecPackage.GUARDED: return createguarded();
			case UMLsecPackage.AUTHORIZEDSTATUS: return createauthorizedstatus();
			case UMLsecPackage.LOCKEDSTATUS: return createlockedstatus();
			case UMLsecPackage.REQUIRES: return createrequires();
			case UMLsecPackage.PRIVACY: return createprivacy();
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
	public UMLsecPackage getUMLsecPackage() {
		return (UMLsecPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static UMLsecPackage getPackage() {
		return UMLsecPackage.eINSTANCE;
	}

} //UMLsecFactoryImpl
