/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.Internet;
import carisma.profile.umlsec.POSdevice;
import carisma.profile.umlsec.SAPTransaction;
import carisma.profile.umlsec.UmlsecFactory;
import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.allowedusers;
import carisma.profile.umlsec.authorizedstatus;
import carisma.profile.umlsec.call;
import carisma.profile.umlsec.critical;
import carisma.profile.umlsec.datasecurity;
import carisma.profile.umlsec.encrypted;
import carisma.profile.umlsec.fairexchange;
import carisma.profile.umlsec.guarded;
import carisma.profile.umlsec.guardedaccess;
import carisma.profile.umlsec.high;
import carisma.profile.umlsec.identifiable;
import carisma.profile.umlsec.integrity;
import carisma.profile.umlsec.issuernode;
import carisma.profile.umlsec.lockedstatus;
import carisma.profile.umlsec.nodownflow;
import carisma.profile.umlsec.noupflow;
import carisma.profile.umlsec.privacy;
import carisma.profile.umlsec.protectedaction;
import carisma.profile.umlsec.provable;
import carisma.profile.umlsec.rbac;
import carisma.profile.umlsec.requires;
import carisma.profile.umlsec.secrecy;
import carisma.profile.umlsec.securedependency;
import carisma.profile.umlsec.securelinks;
import carisma.profile.umlsec.send;
import carisma.profile.umlsec.seperationofduty;
import carisma.profile.umlsec.smartcard;
import carisma.profile.umlsec.usedby;
import carisma.profile.umlsec.wire;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.uml2.types.TypesPackage;

import org.eclipse.uml2.uml.UMLPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class UmlsecPackageImpl extends EPackageImpl implements UmlsecPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass protectedactionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass rbacEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sapTransactionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass allowedusersEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass seperationofdutyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass usedbyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass identifiableEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass securedependencyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass callEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sendEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass secrecyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass integrityEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass highEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass encryptedEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass wireEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass internetEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass lanEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass smartcardEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass poSdeviceEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass issuernodeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass criticalEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass datasecurityEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass securelinksEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass fairexchangeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass provableEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass nodownflowEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass noupflowEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass guardedaccessEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass guardedEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass authorizedstatusEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass lockedstatusEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass requiresEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass privacyEClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see carisma.profile.umlsec.UmlsecPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private UmlsecPackageImpl() {
		super(eNS_URI, UmlsecFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link UmlsecPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static UmlsecPackage init() {
		if (isInited) return (UmlsecPackage)EPackage.Registry.INSTANCE.getEPackage(UmlsecPackage.eNS_URI);

		// Obtain or create and register package
		UmlsecPackageImpl theUmlsecPackage = (UmlsecPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof UmlsecPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new UmlsecPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theUmlsecPackage.createPackageContents();

		// Initialize created meta-data
		theUmlsecPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theUmlsecPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(UmlsecPackage.eNS_URI, theUmlsecPackage);
		return theUmlsecPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getprotectedaction() {
		return protectedactionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getprotectedaction_Permission() {
		return (EAttribute)protectedactionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getprotectedaction_Base_Action() {
		return (EReference)protectedactionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getprotectedaction_Base_State() {
		return (EReference)protectedactionEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getrbac() {
		return rbacEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrbac_Protectedactions() {
		return (EReference)rbacEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrbac_Role() {
		return (EAttribute)rbacEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrbac_Right() {
		return (EAttribute)rbacEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrbac_Base_Package() {
		return (EReference)rbacEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getSAPTransaction() {
		return sapTransactionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getSAPTransaction_Id() {
		return (EAttribute)sapTransactionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getSAPTransaction_Base_Action() {
		return (EReference)sapTransactionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getallowedusers() {
		return allowedusersEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getallowedusers_Base_Action() {
		return (EReference)allowedusersEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getallowedusers_Users() {
		return (EAttribute)allowedusersEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getseperationofduty() {
		return seperationofdutyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getseperationofduty_Base_Action() {
		return (EReference)seperationofdutyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getseperationofduty_Activity() {
		return (EReference)seperationofdutyEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getusedby() {
		return usedbyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getusedby_User() {
		return (EAttribute)usedbyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getusedby_Base_Action() {
		return (EReference)usedbyEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getusedby_Base_State() {
		return (EReference)usedbyEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getidentifiable() {
		return identifiableEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getidentifiable_Id() {
		return (EAttribute)identifiableEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getidentifiable_Base_Element() {
		return (EReference)identifiableEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsecuredependency() {
		return securedependencyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsecuredependency_Base_Package() {
		return (EReference)securedependencyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getcall() {
		return callEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getcall_Base_Dependency() {
		return (EReference)callEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsend() {
		return sendEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsend_Base_Dependency() {
		return (EReference)sendEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsecrecy() {
		return secrecyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsecrecy_Base_Dependency() {
		return (EReference)secrecyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsecrecy_Base_Connector() {
		return (EReference)secrecyEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getintegrity() {
		return integrityEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getintegrity_Base_Dependency() {
		return (EReference)integrityEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getintegrity_Base_Connector() {
		return (EReference)integrityEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass gethigh() {
		return highEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference gethigh_Base_Dependency() {
		return (EReference)highEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference gethigh_Base_Connector() {
		return (EReference)highEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getencrypted() {
		return encryptedEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getencrypted_Base_CommunicationPath() {
		return (EReference)encryptedEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getwire() {
		return wireEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getwire_Base_CommunicationPath() {
		return (EReference)wireEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getInternet() {
		return internetEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getInternet_Base_CommunicationPath() {
		return (EReference)internetEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLAN() {
		return lanEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getLAN_Base_CommunicationPath() {
		return (EReference)lanEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getLAN_Base_Node() {
		return (EReference)lanEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsmartcard() {
		return smartcardEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsmartcard_Base_Node() {
		return (EReference)smartcardEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getPOSdevice() {
		return poSdeviceEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getPOSdevice_Base_Node() {
		return (EReference)poSdeviceEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getissuernode() {
		return issuernodeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getissuernode_Base_Node() {
		return (EReference)issuernodeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getcritical() {
		return criticalEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getcritical_Secrecy() {
		return (EAttribute)criticalEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getcritical_Integrity() {
		return (EAttribute)criticalEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getcritical_High() {
		return (EAttribute)criticalEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getcritical_Base_Class() {
		return (EReference)criticalEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getcritical_Base_Component() {
		return (EReference)criticalEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getcritical_Fresh() {
		return (EAttribute)criticalEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getcritical_Authenticity() {
		return (EAttribute)criticalEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getcritical_Base_InstanceSpecification() {
		return (EReference)criticalEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getcritical_Base_Classifier() {
		return (EReference)criticalEClass.getEStructuralFeatures().get(8);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getcritical_Privacy() {
		return (EAttribute)criticalEClass.getEStructuralFeatures().get(9);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getdatasecurity() {
		return datasecurityEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getdatasecurity_Adversary() {
		return (EAttribute)datasecurityEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getdatasecurity_Base_Package() {
		return (EReference)datasecurityEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getdatasecurity_Authenticity() {
		return (EAttribute)datasecurityEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getdatasecurity_Integrity() {
		return (EAttribute)datasecurityEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsecurelinks() {
		return securelinksEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getsecurelinks_Adversary() {
		return (EAttribute)securelinksEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsecurelinks_Base_Package() {
		return (EReference)securelinksEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getfairexchange() {
		return fairexchangeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getfairexchange_Start() {
		return (EReference)fairexchangeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getfairexchange_Stop() {
		return (EReference)fairexchangeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getfairexchange_Adversary() {
		return (EAttribute)fairexchangeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getfairexchange_Base_Package() {
		return (EReference)fairexchangeEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getprovable() {
		return provableEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getprovable_Action() {
		return (EAttribute)provableEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getprovable_Adversary() {
		return (EAttribute)provableEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getprovable_Cert() {
		return (EAttribute)provableEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getprovable_Base_Package() {
		return (EReference)provableEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getnodownflow() {
		return nodownflowEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getnodownflow_Base_Package() {
		return (EReference)nodownflowEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getnoupflow() {
		return noupflowEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getnoupflow_Base_Package() {
		return (EReference)noupflowEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getguardedaccess() {
		return guardedaccessEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getguardedaccess_Base_Package() {
		return (EReference)guardedaccessEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getguarded() {
		return guardedEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getguarded_Base_Classifier() {
		return (EReference)guardedEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getguarded_Base_InstanceSpecification() {
		return (EReference)guardedEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getguarded_Guard() {
		return (EAttribute)guardedEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getauthorizedstatus() {
		return authorizedstatusEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getauthorizedstatus_Permission() {
		return (EAttribute)authorizedstatusEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getauthorizedstatus_Base_State() {
		return (EReference)authorizedstatusEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getlockedstatus() {
		return lockedstatusEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getlockedstatus_Base_State() {
		return (EReference)lockedstatusEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getrequires() {
		return requiresEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrequires_Base_Action() {
		return (EReference)requiresEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrequires_Actions() {
		return (EReference)requiresEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getprivacy() {
		return privacyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getprivacy_Base_Dependency() {
		return (EReference)privacyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getprivacy_Base_Connector() {
		return (EReference)privacyEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UmlsecFactory getUmlsecFactory() {
		return (UmlsecFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		protectedactionEClass = createEClass(PROTECTEDACTION);
		createEAttribute(protectedactionEClass, PROTECTEDACTION__PERMISSION);
		createEReference(protectedactionEClass, PROTECTEDACTION__BASE_ACTION);
		createEReference(protectedactionEClass, PROTECTEDACTION__BASE_STATE);

		rbacEClass = createEClass(RBAC);
		createEReference(rbacEClass, RBAC__PROTECTEDACTIONS);
		createEAttribute(rbacEClass, RBAC__ROLE);
		createEAttribute(rbacEClass, RBAC__RIGHT);
		createEReference(rbacEClass, RBAC__BASE_PACKAGE);

		sapTransactionEClass = createEClass(SAP_TRANSACTION);
		createEAttribute(sapTransactionEClass, SAP_TRANSACTION__ID);
		createEReference(sapTransactionEClass, SAP_TRANSACTION__BASE_ACTION);

		allowedusersEClass = createEClass(ALLOWEDUSERS);
		createEReference(allowedusersEClass, ALLOWEDUSERS__BASE_ACTION);
		createEAttribute(allowedusersEClass, ALLOWEDUSERS__USERS);

		seperationofdutyEClass = createEClass(SEPERATIONOFDUTY);
		createEReference(seperationofdutyEClass, SEPERATIONOFDUTY__BASE_ACTION);
		createEReference(seperationofdutyEClass, SEPERATIONOFDUTY__ACTIVITY);

		usedbyEClass = createEClass(USEDBY);
		createEAttribute(usedbyEClass, USEDBY__USER);
		createEReference(usedbyEClass, USEDBY__BASE_ACTION);
		createEReference(usedbyEClass, USEDBY__BASE_STATE);

		identifiableEClass = createEClass(IDENTIFIABLE);
		createEAttribute(identifiableEClass, IDENTIFIABLE__ID);
		createEReference(identifiableEClass, IDENTIFIABLE__BASE_ELEMENT);

		securedependencyEClass = createEClass(SECUREDEPENDENCY);
		createEReference(securedependencyEClass, SECUREDEPENDENCY__BASE_PACKAGE);

		callEClass = createEClass(CALL);
		createEReference(callEClass, CALL__BASE_DEPENDENCY);

		sendEClass = createEClass(SEND);
		createEReference(sendEClass, SEND__BASE_DEPENDENCY);

		secrecyEClass = createEClass(SECRECY);
		createEReference(secrecyEClass, SECRECY__BASE_DEPENDENCY);
		createEReference(secrecyEClass, SECRECY__BASE_CONNECTOR);

		integrityEClass = createEClass(INTEGRITY);
		createEReference(integrityEClass, INTEGRITY__BASE_DEPENDENCY);
		createEReference(integrityEClass, INTEGRITY__BASE_CONNECTOR);

		highEClass = createEClass(HIGH);
		createEReference(highEClass, HIGH__BASE_DEPENDENCY);
		createEReference(highEClass, HIGH__BASE_CONNECTOR);

		encryptedEClass = createEClass(ENCRYPTED);
		createEReference(encryptedEClass, ENCRYPTED__BASE_COMMUNICATION_PATH);

		wireEClass = createEClass(WIRE);
		createEReference(wireEClass, WIRE__BASE_COMMUNICATION_PATH);

		internetEClass = createEClass(INTERNET);
		createEReference(internetEClass, INTERNET__BASE_COMMUNICATION_PATH);

		lanEClass = createEClass(LAN);
		createEReference(lanEClass, LAN__BASE_COMMUNICATION_PATH);
		createEReference(lanEClass, LAN__BASE_NODE);

		smartcardEClass = createEClass(SMARTCARD);
		createEReference(smartcardEClass, SMARTCARD__BASE_NODE);

		poSdeviceEClass = createEClass(PO_SDEVICE);
		createEReference(poSdeviceEClass, PO_SDEVICE__BASE_NODE);

		issuernodeEClass = createEClass(ISSUERNODE);
		createEReference(issuernodeEClass, ISSUERNODE__BASE_NODE);

		criticalEClass = createEClass(CRITICAL);
		createEAttribute(criticalEClass, CRITICAL__SECRECY);
		createEAttribute(criticalEClass, CRITICAL__INTEGRITY);
		createEAttribute(criticalEClass, CRITICAL__HIGH);
		createEReference(criticalEClass, CRITICAL__BASE_CLASS);
		createEReference(criticalEClass, CRITICAL__BASE_COMPONENT);
		createEAttribute(criticalEClass, CRITICAL__FRESH);
		createEAttribute(criticalEClass, CRITICAL__AUTHENTICITY);
		createEReference(criticalEClass, CRITICAL__BASE_INSTANCE_SPECIFICATION);
		createEReference(criticalEClass, CRITICAL__BASE_CLASSIFIER);
		createEAttribute(criticalEClass, CRITICAL__PRIVACY);

		datasecurityEClass = createEClass(DATASECURITY);
		createEAttribute(datasecurityEClass, DATASECURITY__ADVERSARY);
		createEReference(datasecurityEClass, DATASECURITY__BASE_PACKAGE);
		createEAttribute(datasecurityEClass, DATASECURITY__AUTHENTICITY);
		createEAttribute(datasecurityEClass, DATASECURITY__INTEGRITY);

		securelinksEClass = createEClass(SECURELINKS);
		createEAttribute(securelinksEClass, SECURELINKS__ADVERSARY);
		createEReference(securelinksEClass, SECURELINKS__BASE_PACKAGE);

		fairexchangeEClass = createEClass(FAIREXCHANGE);
		createEReference(fairexchangeEClass, FAIREXCHANGE__START);
		createEReference(fairexchangeEClass, FAIREXCHANGE__STOP);
		createEAttribute(fairexchangeEClass, FAIREXCHANGE__ADVERSARY);
		createEReference(fairexchangeEClass, FAIREXCHANGE__BASE_PACKAGE);

		provableEClass = createEClass(PROVABLE);
		createEAttribute(provableEClass, PROVABLE__ACTION);
		createEAttribute(provableEClass, PROVABLE__ADVERSARY);
		createEAttribute(provableEClass, PROVABLE__CERT);
		createEReference(provableEClass, PROVABLE__BASE_PACKAGE);

		nodownflowEClass = createEClass(NODOWNFLOW);
		createEReference(nodownflowEClass, NODOWNFLOW__BASE_PACKAGE);

		noupflowEClass = createEClass(NOUPFLOW);
		createEReference(noupflowEClass, NOUPFLOW__BASE_PACKAGE);

		guardedaccessEClass = createEClass(GUARDEDACCESS);
		createEReference(guardedaccessEClass, GUARDEDACCESS__BASE_PACKAGE);

		guardedEClass = createEClass(GUARDED);
		createEReference(guardedEClass, GUARDED__BASE_CLASSIFIER);
		createEReference(guardedEClass, GUARDED__BASE_INSTANCE_SPECIFICATION);
		createEAttribute(guardedEClass, GUARDED__GUARD);

		authorizedstatusEClass = createEClass(AUTHORIZEDSTATUS);
		createEAttribute(authorizedstatusEClass, AUTHORIZEDSTATUS__PERMISSION);
		createEReference(authorizedstatusEClass, AUTHORIZEDSTATUS__BASE_STATE);

		lockedstatusEClass = createEClass(LOCKEDSTATUS);
		createEReference(lockedstatusEClass, LOCKEDSTATUS__BASE_STATE);

		requiresEClass = createEClass(REQUIRES);
		createEReference(requiresEClass, REQUIRES__BASE_ACTION);
		createEReference(requiresEClass, REQUIRES__ACTIONS);

		privacyEClass = createEClass(PRIVACY);
		createEReference(privacyEClass, PRIVACY__BASE_DEPENDENCY);
		createEReference(privacyEClass, PRIVACY__BASE_CONNECTOR);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);
		UMLPackage theUMLPackage = (UMLPackage)EPackage.Registry.INSTANCE.getEPackage(UMLPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes

		// Initialize classes, features, and operations; add parameters
		initEClass(protectedactionEClass, protectedaction.class, "protectedaction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getprotectedaction_Permission(), theTypesPackage.getString(), "permission", null, 1, 1, protectedaction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getprotectedaction_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 1, 1, protectedaction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getprotectedaction_Base_State(), theUMLPackage.getState(), null, "base_State", null, 1, 1, protectedaction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(rbacEClass, rbac.class, "rbac", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getrbac_Protectedactions(), theUMLPackage.getAction(), null, "protectedactions", null, 0, -1, rbac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrbac_Role(), theTypesPackage.getString(), "role", null, 1, -1, rbac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrbac_Right(), theTypesPackage.getString(), "right", null, 0, -1, rbac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrbac_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, rbac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(sapTransactionEClass, SAPTransaction.class, "SAPTransaction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSAPTransaction_Id(), theTypesPackage.getString(), "id", null, 1, 1, SAPTransaction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getSAPTransaction_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 1, 1, SAPTransaction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(allowedusersEClass, allowedusers.class, "allowedusers", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getallowedusers_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 1, 1, allowedusers.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getallowedusers_Users(), theTypesPackage.getString(), "users", null, 1, -1, allowedusers.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(seperationofdutyEClass, seperationofduty.class, "seperationofduty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getseperationofduty_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 1, 1, seperationofduty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getseperationofduty_Activity(), theUMLPackage.getAction(), null, "activity", null, 1, -1, seperationofduty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(usedbyEClass, usedby.class, "usedby", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getusedby_User(), theTypesPackage.getString(), "user", null, 1, -1, usedby.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getusedby_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 1, 1, usedby.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getusedby_Base_State(), theUMLPackage.getState(), null, "base_State", null, 1, 1, usedby.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(identifiableEClass, identifiable.class, "identifiable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getidentifiable_Id(), theTypesPackage.getString(), "id", null, 1, 1, identifiable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getidentifiable_Base_Element(), theUMLPackage.getElement(), null, "base_Element", null, 1, 1, identifiable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(securedependencyEClass, securedependency.class, "securedependency", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getsecuredependency_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, securedependency.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(callEClass, call.class, "call", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getcall_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, call.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(sendEClass, send.class, "send", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getsend_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, send.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(secrecyEClass, secrecy.class, "secrecy", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getsecrecy_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, secrecy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getsecrecy_Base_Connector(), theUMLPackage.getConnector(), null, "base_Connector", null, 1, 1, secrecy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(integrityEClass, integrity.class, "integrity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getintegrity_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, integrity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getintegrity_Base_Connector(), theUMLPackage.getConnector(), null, "base_Connector", null, 1, 1, integrity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(highEClass, high.class, "high", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(gethigh_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, high.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(gethigh_Base_Connector(), theUMLPackage.getConnector(), null, "base_Connector", null, 1, 1, high.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(encryptedEClass, encrypted.class, "encrypted", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getencrypted_Base_CommunicationPath(), theUMLPackage.getCommunicationPath(), null, "base_CommunicationPath", null, 1, 1, encrypted.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(wireEClass, wire.class, "wire", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getwire_Base_CommunicationPath(), theUMLPackage.getCommunicationPath(), null, "base_CommunicationPath", null, 1, 1, wire.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(internetEClass, Internet.class, "Internet", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getInternet_Base_CommunicationPath(), theUMLPackage.getCommunicationPath(), null, "base_CommunicationPath", null, 1, 1, Internet.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(lanEClass, carisma.profile.umlsec.LAN.class, "LAN", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getLAN_Base_CommunicationPath(), theUMLPackage.getCommunicationPath(), null, "base_CommunicationPath", null, 1, 1, carisma.profile.umlsec.LAN.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getLAN_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, carisma.profile.umlsec.LAN.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(smartcardEClass, smartcard.class, "smartcard", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getsmartcard_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, smartcard.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(poSdeviceEClass, POSdevice.class, "POSdevice", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getPOSdevice_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, POSdevice.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(issuernodeEClass, issuernode.class, "issuernode", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getissuernode_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, issuernode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(criticalEClass, critical.class, "critical", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getcritical_Secrecy(), theTypesPackage.getString(), "secrecy", null, 0, -1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getcritical_Integrity(), theTypesPackage.getString(), "integrity", null, 0, -1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getcritical_High(), theTypesPackage.getString(), "high", null, 0, -1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getcritical_Base_Class(), theUMLPackage.getClass_(), null, "base_Class", null, 1, 1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getcritical_Base_Component(), theUMLPackage.getComponent(), null, "base_Component", null, 1, 1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getcritical_Fresh(), theTypesPackage.getString(), "fresh", null, 0, -1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getcritical_Authenticity(), theTypesPackage.getString(), "authenticity", null, 0, -1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getcritical_Base_InstanceSpecification(), theUMLPackage.getInstanceSpecification(), null, "base_InstanceSpecification", null, 1, 1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getcritical_Base_Classifier(), theUMLPackage.getClassifier(), null, "base_Classifier", null, 1, 1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getcritical_Privacy(), theTypesPackage.getString(), "privacy", null, 0, -1, critical.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(datasecurityEClass, datasecurity.class, "datasecurity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getdatasecurity_Adversary(), theTypesPackage.getString(), "adversary", null, 1, 1, datasecurity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatasecurity_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, datasecurity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getdatasecurity_Authenticity(), theTypesPackage.getString(), "authenticity", null, 1, 1, datasecurity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getdatasecurity_Integrity(), theTypesPackage.getString(), "integrity", null, 1, 1, datasecurity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(securelinksEClass, securelinks.class, "securelinks", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getsecurelinks_Adversary(), theTypesPackage.getString(), "adversary", null, 1, 1, securelinks.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getsecurelinks_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, securelinks.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(fairexchangeEClass, fairexchange.class, "fairexchange", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getfairexchange_Start(), theUMLPackage.getAction(), null, "start", null, 1, -1, fairexchange.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getfairexchange_Stop(), theUMLPackage.getAction(), null, "stop", null, 1, -1, fairexchange.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getfairexchange_Adversary(), theTypesPackage.getString(), "adversary", null, 1, 1, fairexchange.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getfairexchange_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, fairexchange.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(provableEClass, provable.class, "provable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getprovable_Action(), theTypesPackage.getString(), "action", null, 1, 1, provable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getprovable_Adversary(), theTypesPackage.getString(), "adversary", null, 1, 1, provable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getprovable_Cert(), theTypesPackage.getString(), "cert", null, 1, 1, provable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getprovable_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, provable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(nodownflowEClass, nodownflow.class, "nodownflow", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getnodownflow_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, nodownflow.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(noupflowEClass, noupflow.class, "noupflow", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getnoupflow_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, noupflow.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(guardedaccessEClass, guardedaccess.class, "guardedaccess", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getguardedaccess_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, guardedaccess.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(guardedEClass, guarded.class, "guarded", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getguarded_Base_Classifier(), theUMLPackage.getClassifier(), null, "base_Classifier", null, 1, 1, guarded.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getguarded_Base_InstanceSpecification(), theUMLPackage.getInstanceSpecification(), null, "base_InstanceSpecification", null, 1, 1, guarded.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getguarded_Guard(), theTypesPackage.getString(), "guard", null, 0, -1, guarded.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(authorizedstatusEClass, authorizedstatus.class, "authorizedstatus", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getauthorizedstatus_Permission(), theTypesPackage.getString(), "permission", null, 1, 1, authorizedstatus.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getauthorizedstatus_Base_State(), theUMLPackage.getState(), null, "base_State", null, 1, 1, authorizedstatus.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(lockedstatusEClass, lockedstatus.class, "lockedstatus", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getlockedstatus_Base_State(), theUMLPackage.getState(), null, "base_State", null, 1, 1, lockedstatus.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(requiresEClass, requires.class, "requires", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getrequires_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 1, 1, requires.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrequires_Actions(), theUMLPackage.getAction(), null, "actions", null, 1, -1, requires.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(privacyEClass, privacy.class, "privacy", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getprivacy_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, privacy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getprivacy_Base_Connector(), theUMLPackage.getConnector(), null, "base_Connector", null, 1, 1, privacy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http://www.eclipse.org/uml2/2.0.0/UML
		createUMLAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http://www.eclipse.org/uml2/2.0.0/UML</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createUMLAnnotations() {
		String source = "http://www.eclipse.org/uml2/2.0.0/UML";	
		addAnnotation
		  (this, 
		   source, 
		   new String[] {
			 "originalName", "UMLsec"
		   });	
		addAnnotation
		  (protectedactionEClass, 
		   source, 
		   new String[] {
			 "originalName", "protected action"
		   });	
		addAnnotation
		  (getrbac_Protectedactions(), 
		   source, 
		   new String[] {
			 "originalName", "protected actions"
		   });	
		addAnnotation
		  (sapTransactionEClass, 
		   source, 
		   new String[] {
			 "originalName", "SAP Transaction"
		   });	
		addAnnotation
		  (allowedusersEClass, 
		   source, 
		   new String[] {
			 "originalName", "allowed users"
		   });	
		addAnnotation
		  (seperationofdutyEClass, 
		   source, 
		   new String[] {
			 "originalName", "seperation of duty"
		   });	
		addAnnotation
		  (usedbyEClass, 
		   source, 
		   new String[] {
			 "originalName", "used-by"
		   });	
		addAnnotation
		  (securedependencyEClass, 
		   source, 
		   new String[] {
			 "originalName", "secure dependency"
		   });	
		addAnnotation
		  (smartcardEClass, 
		   source, 
		   new String[] {
			 "originalName", "smart card"
		   });	
		addAnnotation
		  (poSdeviceEClass, 
		   source, 
		   new String[] {
			 "originalName", "POS device"
		   });	
		addAnnotation
		  (issuernodeEClass, 
		   source, 
		   new String[] {
			 "originalName", "issuer node"
		   });	
		addAnnotation
		  (datasecurityEClass, 
		   source, 
		   new String[] {
			 "originalName", "data security"
		   });	
		addAnnotation
		  (securelinksEClass, 
		   source, 
		   new String[] {
			 "originalName", "secure links"
		   });	
		addAnnotation
		  (fairexchangeEClass, 
		   source, 
		   new String[] {
			 "originalName", "fair exchange"
		   });	
		addAnnotation
		  (nodownflowEClass, 
		   source, 
		   new String[] {
			 "originalName", "no down-flow"
		   });	
		addAnnotation
		  (noupflowEClass, 
		   source, 
		   new String[] {
			 "originalName", "no up-flow"
		   });	
		addAnnotation
		  (guardedaccessEClass, 
		   source, 
		   new String[] {
			 "originalName", "guarded access"
		   });	
		addAnnotation
		  (authorizedstatusEClass, 
		   source, 
		   new String[] {
			 "originalName", "authorized-status"
		   });	
		addAnnotation
		  (lockedstatusEClass, 
		   source, 
		   new String[] {
			 "originalName", "locked-status"
		   });
	}

} //UmlsecPackageImpl
