/**
 */
package carisma.profile.umlsec.extension4ids.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.uml2.types.TypesPackage;

import org.eclipse.uml2.uml.UMLPackage;

import carisma.profile.umlsec.extension4ids.ConsumerConnector;
import carisma.profile.umlsec.extension4ids.Extension4idsFactory;
import carisma.profile.umlsec.extension4ids.Extension4idsPackage;
import carisma.profile.umlsec.extension4ids.IDSconnector;
import carisma.profile.umlsec.extension4ids.ProviderConnector;
import carisma.profile.umlsec.extension4ids.TransferProcessProtocol;
import carisma.profile.umlsec.extension4ids.TransferType;
import carisma.profile.umlsec.extension4ids.UsageControl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class Extension4idsPackageImpl extends EPackageImpl implements Extension4idsPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass idSconnectorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass usageControlEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass transferProcessProtocolEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass providerConnectorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass consumerConnectorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum transferTypeEEnum = null;

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
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private Extension4idsPackageImpl() {
		super(eNS_URI, Extension4idsFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link Extension4idsPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static Extension4idsPackage init() {
		if (isInited) return (Extension4idsPackage)EPackage.Registry.INSTANCE.getEPackage(Extension4idsPackage.eNS_URI);

		// Obtain or create and register package
		Object registeredExtension4idsPackage = EPackage.Registry.INSTANCE.get(eNS_URI);
		Extension4idsPackageImpl theExtension4idsPackage = registeredExtension4idsPackage instanceof Extension4idsPackageImpl ? (Extension4idsPackageImpl)registeredExtension4idsPackage : new Extension4idsPackageImpl();

		isInited = true;

		// Initialize simple dependencies
		EcorePackage.eINSTANCE.eClass();
		TypesPackage.eINSTANCE.eClass();
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theExtension4idsPackage.createPackageContents();

		// Initialize created meta-data
		theExtension4idsPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theExtension4idsPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(Extension4idsPackage.eNS_URI, theExtension4idsPackage);
		return theExtension4idsPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getIDSconnector() {
		return idSconnectorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getIDSconnector_Base_Artifact() {
		return (EReference)idSconnectorEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getUsageControl() {
		return usageControlEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getUsageControl_Base_Dependency() {
		return (EReference)usageControlEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getTransferProcessProtocol() {
		return transferProcessProtocolEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTransferProcessProtocol_Type() {
		return (EAttribute)transferProcessProtocolEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Transfer_req_step() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Transfer_start_step() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Push_pull_step() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Transfer_complete_step() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Transfer_suspend_step() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Transfer_terminate_step() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTransferProcessProtocol_Base_Interaction() {
		return (EReference)transferProcessProtocolEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getProviderConnector() {
		return providerConnectorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getProviderConnector_Base_Lifeline() {
		return (EReference)providerConnectorEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getConsumerConnector() {
		return consumerConnectorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getConsumerConnector_Base_Lifeline() {
		return (EReference)consumerConnectorEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getTransferType() {
		return transferTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Extension4idsFactory getExtension4idsFactory() {
		return (Extension4idsFactory)getEFactoryInstance();
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
		idSconnectorEClass = createEClass(ID_SCONNECTOR);
		createEReference(idSconnectorEClass, ID_SCONNECTOR__BASE_ARTIFACT);

		usageControlEClass = createEClass(USAGE_CONTROL);
		createEReference(usageControlEClass, USAGE_CONTROL__BASE_DEPENDENCY);

		transferProcessProtocolEClass = createEClass(TRANSFER_PROCESS_PROTOCOL);
		createEAttribute(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__TYPE);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__TRANSFER_REQ_STEP);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__TRANSFER_START_STEP);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__PUSH_PULL_STEP);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__TRANSFER_COMPLETE_STEP);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__TRANSFER_SUSPEND_STEP);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__TRANSFER_TERMINATE_STEP);
		createEReference(transferProcessProtocolEClass, TRANSFER_PROCESS_PROTOCOL__BASE_INTERACTION);

		providerConnectorEClass = createEClass(PROVIDER_CONNECTOR);
		createEReference(providerConnectorEClass, PROVIDER_CONNECTOR__BASE_LIFELINE);

		consumerConnectorEClass = createEClass(CONSUMER_CONNECTOR);
		createEReference(consumerConnectorEClass, CONSUMER_CONNECTOR__BASE_LIFELINE);

		// Create enums
		transferTypeEEnum = createEEnum(TRANSFER_TYPE);
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

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes

		// Initialize classes, features, and operations; add parameters
		initEClass(idSconnectorEClass, IDSconnector.class, "IDSconnector", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getIDSconnector_Base_Artifact(), theUMLPackage.getArtifact(), null, "base_Artifact", null, 0, 1, IDSconnector.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(usageControlEClass, UsageControl.class, "UsageControl", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getUsageControl_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 0, 1, UsageControl.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(transferProcessProtocolEClass, TransferProcessProtocol.class, "TransferProcessProtocol", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTransferProcessProtocol_Type(), this.getTransferType(), "type", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Transfer_req_step(), theUMLPackage.getMessage(), null, "transfer_req_step", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Transfer_start_step(), theUMLPackage.getMessage(), null, "transfer_start_step", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Push_pull_step(), theUMLPackage.getMessage(), null, "push_pull_step", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Transfer_complete_step(), theUMLPackage.getMessage(), null, "transfer_complete_step", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Transfer_suspend_step(), theUMLPackage.getMessage(), null, "transfer_suspend_step", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Transfer_terminate_step(), theUMLPackage.getMessage(), null, "transfer_terminate_step", null, 1, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getTransferProcessProtocol_Base_Interaction(), theUMLPackage.getInteraction(), null, "base_Interaction", null, 0, 1, TransferProcessProtocol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(providerConnectorEClass, ProviderConnector.class, "ProviderConnector", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getProviderConnector_Base_Lifeline(), theUMLPackage.getLifeline(), null, "base_Lifeline", null, 0, 1, ProviderConnector.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(consumerConnectorEClass, ConsumerConnector.class, "ConsumerConnector", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getConsumerConnector_Base_Lifeline(), theUMLPackage.getLifeline(), null, "base_Lifeline", null, 0, 1, ConsumerConnector.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(transferTypeEEnum, TransferType.class, "TransferType");
		addEEnumLiteral(transferTypeEEnum, TransferType.PUSH);
		addEEnumLiteral(transferTypeEEnum, TransferType.PULL);

		// Create resource
		createResource(eNS_URI);
	}

} //Extension4idsPackageImpl
