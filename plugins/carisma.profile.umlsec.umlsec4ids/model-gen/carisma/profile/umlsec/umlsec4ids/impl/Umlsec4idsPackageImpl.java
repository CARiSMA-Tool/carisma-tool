/**
 */
package carisma.profile.umlsec.umlsec4ids.impl;

import carisma.profile.umlsec.umlsec4ids.Consumer;
import carisma.profile.umlsec.umlsec4ids.Owner;
import carisma.profile.umlsec.umlsec4ids.Umlsec4idsFactory;
import carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage;
import carisma.profile.umlsec.umlsec4ids.base;
import carisma.profile.umlsec.umlsec4ids.basefree;

import carisma.profile.umlsec.umlsec4ids.certified;
import carisma.profile.umlsec.umlsec4ids.dataprovenancetracking;
import carisma.profile.umlsec.umlsec4ids.datausagecontrol;
import carisma.profile.umlsec.umlsec4ids.encryption;
import carisma.profile.umlsec.umlsec4ids.isolated;
import carisma.profile.umlsec.umlsec4ids.trust;
import carisma.profile.umlsec.umlsec4ids.trustplus;
import carisma.profile.umlsec.umlsec4ids.verified;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.uml2.types.TypesPackage;

import org.eclipse.uml2.uml.UMLPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class Umlsec4idsPackageImpl extends EPackageImpl implements Umlsec4idsPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass basefreeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass baseEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass trustEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass trustplusEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass dataprovenancetrackingEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass ownerEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass consumerEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass x509EClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass x509TLSEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass datausagecontrolEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass verifiedEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass isolatedEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass encryptionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass certifiedEClass = null;

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
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private Umlsec4idsPackageImpl() {
		super(eNS_URI, Umlsec4idsFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link Umlsec4idsPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static Umlsec4idsPackage init() {
		if (isInited) return (Umlsec4idsPackage)EPackage.Registry.INSTANCE.getEPackage(Umlsec4idsPackage.eNS_URI);

		// Obtain or create and register package
		Object registeredUmlsec4idsPackage = EPackage.Registry.INSTANCE.get(eNS_URI);
		Umlsec4idsPackageImpl theUmlsec4idsPackage = registeredUmlsec4idsPackage instanceof Umlsec4idsPackageImpl ? (Umlsec4idsPackageImpl)registeredUmlsec4idsPackage : new Umlsec4idsPackageImpl();

		isInited = true;

		// Initialize simple dependencies
		EcorePackage.eINSTANCE.eClass();
		TypesPackage.eINSTANCE.eClass();
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theUmlsec4idsPackage.createPackageContents();

		// Initialize created meta-data
		theUmlsec4idsPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theUmlsec4idsPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(Umlsec4idsPackage.eNS_URI, theUmlsec4idsPackage);
		return theUmlsec4idsPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getbasefree() {
		return basefreeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getbasefree_Base_Node() {
		return (EReference)basefreeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getbase() {
		return baseEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getbase_Base_Node() {
		return (EReference)baseEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass gettrust() {
		return trustEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference gettrust_Base_Node() {
		return (EReference)trustEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass gettrustplus() {
		return trustplusEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference gettrustplus_Base_Node() {
		return (EReference)trustplusEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getdataprovenancetracking() {
		return dataprovenancetrackingEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdataprovenancetracking_Base_Activity() {
		return (EReference)dataprovenancetrackingEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdataprovenancetracking_Start_action() {
		return (EReference)dataprovenancetrackingEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdataprovenancetracking_Stop_action() {
		return (EReference)dataprovenancetrackingEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdataprovenancetracking_Clearing_house() {
		return (EReference)dataprovenancetrackingEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdataprovenancetracking_Protected() {
		return (EReference)dataprovenancetrackingEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getOwner() {
		return ownerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getOwner_Base_ActivityPartition() {
		return (EReference)ownerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getOwner_Protected() {
		return (EReference)ownerEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getOwner_Requested_attributes() {
		return (EAttribute)ownerEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getOwner_Requested_actions() {
		return (EAttribute)ownerEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getConsumer() {
		return consumerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getConsumer_Base_ActivityPartition() {
		return (EReference)consumerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConsumer_Attributes() {
		return (EAttribute)consumerEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConsumer_Actions() {
		return (EAttribute)consumerEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getX509() {
		return x509EClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getX509_Base_Node() {
		return (EReference)x509EClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getX509_Expiration_date_yyyy_mm_dd() {
		return (EAttribute)x509EClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getX509TLS() {
		return x509TLSEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getX509TLS_Base_Node() {
		return (EReference)x509TLSEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getX509TLS_Expiration_date_yyyy_mm_dd() {
		return (EAttribute)x509TLSEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getdatausagecontrol() {
		return datausagecontrolEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatausagecontrol_Base_ActivityPartition() {
		return (EReference)datausagecontrolEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatausagecontrol_Permission() {
		return (EReference)datausagecontrolEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatausagecontrol_Obligation_start() {
		return (EReference)datausagecontrolEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatausagecontrol_Obligation_stop() {
		return (EReference)datausagecontrolEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatausagecontrol_Prohibition() {
		return (EReference)datausagecontrolEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getverified() {
		return verifiedEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getverified_Base_Node() {
		return (EReference)verifiedEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getisolated() {
		return isolatedEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getisolated_Base_Node() {
		return (EReference)isolatedEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getencryption() {
		return encryptionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getencryption_Base_Node() {
		return (EReference)encryptionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getcertified() {
		return certifiedEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getcertified_Base_Node() {
		return (EReference)certifiedEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Umlsec4idsFactory getUmlsec4idsFactory() {
		return (Umlsec4idsFactory)getEFactoryInstance();
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
		basefreeEClass = createEClass(BASEFREE);
		createEReference(basefreeEClass, BASEFREE__BASE_NODE);

		baseEClass = createEClass(BASE);
		createEReference(baseEClass, BASE__BASE_NODE);

		trustEClass = createEClass(TRUST);
		createEReference(trustEClass, TRUST__BASE_NODE);

		trustplusEClass = createEClass(TRUSTPLUS);
		createEReference(trustplusEClass, TRUSTPLUS__BASE_NODE);

		dataprovenancetrackingEClass = createEClass(DATAPROVENANCETRACKING);
		createEReference(dataprovenancetrackingEClass, DATAPROVENANCETRACKING__BASE_ACTIVITY);
		createEReference(dataprovenancetrackingEClass, DATAPROVENANCETRACKING__START_ACTION);
		createEReference(dataprovenancetrackingEClass, DATAPROVENANCETRACKING__STOP_ACTION);
		createEReference(dataprovenancetrackingEClass, DATAPROVENANCETRACKING__CLEARING_HOUSE);
		createEReference(dataprovenancetrackingEClass, DATAPROVENANCETRACKING__PROTECTED);

		ownerEClass = createEClass(OWNER);
		createEReference(ownerEClass, OWNER__BASE_ACTIVITY_PARTITION);
		createEReference(ownerEClass, OWNER__PROTECTED);
		createEAttribute(ownerEClass, OWNER__REQUESTED_ATTRIBUTES);
		createEAttribute(ownerEClass, OWNER__REQUESTED_ACTIONS);

		consumerEClass = createEClass(CONSUMER);
		createEReference(consumerEClass, CONSUMER__BASE_ACTIVITY_PARTITION);
		createEAttribute(consumerEClass, CONSUMER__ATTRIBUTES);
		createEAttribute(consumerEClass, CONSUMER__ACTIONS);

		x509EClass = createEClass(X509);
		createEReference(x509EClass, X509__BASE_NODE);
		createEAttribute(x509EClass, X509__EXPIRATION_DATE_YYYY_MM_DD);

		x509TLSEClass = createEClass(X509TLS);
		createEReference(x509TLSEClass, X509TLS__BASE_NODE);
		createEAttribute(x509TLSEClass, X509TLS__EXPIRATION_DATE_YYYY_MM_DD);

		datausagecontrolEClass = createEClass(DATAUSAGECONTROL);
		createEReference(datausagecontrolEClass, DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION);
		createEReference(datausagecontrolEClass, DATAUSAGECONTROL__PERMISSION);
		createEReference(datausagecontrolEClass, DATAUSAGECONTROL__OBLIGATION_START);
		createEReference(datausagecontrolEClass, DATAUSAGECONTROL__OBLIGATION_STOP);
		createEReference(datausagecontrolEClass, DATAUSAGECONTROL__PROHIBITION);

		verifiedEClass = createEClass(VERIFIED);
		createEReference(verifiedEClass, VERIFIED__BASE_NODE);

		isolatedEClass = createEClass(ISOLATED);
		createEReference(isolatedEClass, ISOLATED__BASE_NODE);

		encryptionEClass = createEClass(ENCRYPTION);
		createEReference(encryptionEClass, ENCRYPTION__BASE_NODE);

		certifiedEClass = createEClass(CERTIFIED);
		createEReference(certifiedEClass, CERTIFIED__BASE_NODE);
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
		UMLPackage theUMLPackage = (UMLPackage)EPackage.Registry.INSTANCE.getEPackage(UMLPackage.eNS_URI);
		TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes

		// Initialize classes, features, and operations; add parameters
		initEClass(basefreeEClass, basefree.class, "basefree", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getbasefree_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, basefree.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(baseEClass, base.class, "base", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getbase_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, base.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(trustEClass, trust.class, "trust", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(gettrust_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, trust.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(trustplusEClass, trustplus.class, "trustplus", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(gettrustplus_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, trustplus.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(dataprovenancetrackingEClass, dataprovenancetracking.class, "dataprovenancetracking", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getdataprovenancetracking_Base_Activity(), theUMLPackage.getActivity(), null, "base_Activity", null, 1, 1, dataprovenancetracking.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdataprovenancetracking_Start_action(), theUMLPackage.getAction(), null, "start_action", null, 1, -1, dataprovenancetracking.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdataprovenancetracking_Stop_action(), theUMLPackage.getAction(), null, "stop_action", null, 1, -1, dataprovenancetracking.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdataprovenancetracking_Clearing_house(), theUMLPackage.getActivityPartition(), null, "clearing_house", null, 1, -1, dataprovenancetracking.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdataprovenancetracking_Protected(), theUMLPackage.getAction(), null, "protected", null, 1, -1, dataprovenancetracking.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(ownerEClass, Owner.class, "Owner", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getOwner_Base_ActivityPartition(), theUMLPackage.getActivityPartition(), null, "base_ActivityPartition", null, 1, 1, Owner.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getOwner_Protected(), theUMLPackage.getAction(), null, "protected", null, 1, -1, Owner.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getOwner_Requested_attributes(), theTypesPackage.getString(), "requested_attributes", null, 1, -1, Owner.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getOwner_Requested_actions(), theTypesPackage.getString(), "requested_actions", null, 1, -1, Owner.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(consumerEClass, Consumer.class, "Consumer", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getConsumer_Base_ActivityPartition(), theUMLPackage.getActivityPartition(), null, "base_ActivityPartition", null, 1, 1, Consumer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConsumer_Attributes(), theTypesPackage.getString(), "attributes", null, 1, -1, Consumer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConsumer_Actions(), theTypesPackage.getString(), "actions", null, 1, -1, Consumer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(x509EClass, carisma.profile.umlsec.umlsec4ids.X509.class, "X509", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getX509_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, carisma.profile.umlsec.umlsec4ids.X509.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getX509_Expiration_date_yyyy_mm_dd(), theTypesPackage.getInteger(), "expiration_date_yyyy_mm_dd", null, 1, 1, carisma.profile.umlsec.umlsec4ids.X509.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(x509TLSEClass, carisma.profile.umlsec.umlsec4ids.X509TLS.class, "X509TLS", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getX509TLS_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, carisma.profile.umlsec.umlsec4ids.X509TLS.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getX509TLS_Expiration_date_yyyy_mm_dd(), theTypesPackage.getInteger(), "expiration_date_yyyy_mm_dd", null, 1, 1, carisma.profile.umlsec.umlsec4ids.X509TLS.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(datausagecontrolEClass, datausagecontrol.class, "datausagecontrol", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getdatausagecontrol_Base_ActivityPartition(), theUMLPackage.getActivityPartition(), null, "base_ActivityPartition", null, 1, 1, datausagecontrol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatausagecontrol_Permission(), theUMLPackage.getAction(), null, "permission", null, 1, -1, datausagecontrol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatausagecontrol_Obligation_start(), theUMLPackage.getAction(), null, "obligation_start", null, 1, -1, datausagecontrol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatausagecontrol_Obligation_stop(), theUMLPackage.getAction(), null, "obligation_stop", null, 1, -1, datausagecontrol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatausagecontrol_Prohibition(), theUMLPackage.getAction(), null, "prohibition", null, 1, -1, datausagecontrol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(verifiedEClass, verified.class, "verified", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getverified_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, verified.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(isolatedEClass, isolated.class, "isolated", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getisolated_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, isolated.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(encryptionEClass, encryption.class, "encryption", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getencryption_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, encryption.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(certifiedEClass, certified.class, "certified", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getcertified_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 1, 1, certified.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

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
			   "originalName", "umlsec4ids"
		   });
		addAnnotation
		  (dataprovenancetrackingEClass,
		   source,
		   new String[] {
			   "originalName", "Data Provenance Tracking"
		   });
		addAnnotation
		  (datausagecontrolEClass,
		   source,
		   new String[] {
			   "originalName", "Data Usage Control"
		   });
	}

} //Umlsec4idsPackageImpl
