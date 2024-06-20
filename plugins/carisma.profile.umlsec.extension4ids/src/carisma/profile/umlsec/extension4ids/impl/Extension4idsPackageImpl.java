/**
 */
package carisma.profile.umlsec.extension4ids.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.uml2.types.TypesPackage;

import org.eclipse.uml2.uml.UMLPackage;

import carisma.profile.umlsec.extension4ids.Extension4idsFactory;
import carisma.profile.umlsec.extension4ids.Extension4idsPackage;
import carisma.profile.umlsec.extension4ids.IDSconnector;
import carisma.profile.umlsec.extension4ids.consumer;
import carisma.profile.umlsec.extension4ids.datatransfer;
import carisma.profile.umlsec.extension4ids.provider;
import carisma.profile.umlsec.extension4ids.usagecontrol;

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
	private EClass usagecontrolEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass datatransferEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass providerEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass consumerEClass = null;

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
	public EClass getusagecontrol() {
		return usagecontrolEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getusagecontrol_Base_Dependency() {
		return (EReference)usagecontrolEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getdatatransfer() {
		return datatransferEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getdatatransfer_Type() {
		return (EAttribute)datatransferEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Transfer_req_step() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Transfer_start_step() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Push_pull_step() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Transfer_complete_step() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Transfer_suspend_step() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Transfer_terminate_step() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getdatatransfer_Base_Interaction() {
		return (EReference)datatransferEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getprovider() {
		return providerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getprovider_Base_Lifeline() {
		return (EReference)providerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getconsumer() {
		return consumerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getconsumer_Base_Lifeline() {
		return (EReference)consumerEClass.getEStructuralFeatures().get(0);
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

		usagecontrolEClass = createEClass(USAGECONTROL);
		createEReference(usagecontrolEClass, USAGECONTROL__BASE_DEPENDENCY);

		datatransferEClass = createEClass(DATATRANSFER);
		createEAttribute(datatransferEClass, DATATRANSFER__TYPE);
		createEReference(datatransferEClass, DATATRANSFER__TRANSFER_REQ_STEP);
		createEReference(datatransferEClass, DATATRANSFER__TRANSFER_START_STEP);
		createEReference(datatransferEClass, DATATRANSFER__PUSH_PULL_STEP);
		createEReference(datatransferEClass, DATATRANSFER__TRANSFER_COMPLETE_STEP);
		createEReference(datatransferEClass, DATATRANSFER__TRANSFER_SUSPEND_STEP);
		createEReference(datatransferEClass, DATATRANSFER__TRANSFER_TERMINATE_STEP);
		createEReference(datatransferEClass, DATATRANSFER__BASE_INTERACTION);

		providerEClass = createEClass(PROVIDER);
		createEReference(providerEClass, PROVIDER__BASE_LIFELINE);

		consumerEClass = createEClass(CONSUMER);
		createEReference(consumerEClass, CONSUMER__BASE_LIFELINE);
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

		initEClass(usagecontrolEClass, usagecontrol.class, "usagecontrol", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getusagecontrol_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 0, 1, usagecontrol.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(datatransferEClass, datatransfer.class, "datatransfer", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getdatatransfer_Type(), ecorePackage.getEString(), "type", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Transfer_req_step(), theUMLPackage.getMessage(), null, "transfer_req_step", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Transfer_start_step(), theUMLPackage.getMessage(), null, "transfer_start_step", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Push_pull_step(), theUMLPackage.getMessage(), null, "push_pull_step", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Transfer_complete_step(), theUMLPackage.getMessage(), null, "transfer_complete_step", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Transfer_suspend_step(), theUMLPackage.getMessage(), null, "transfer_suspend_step", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Transfer_terminate_step(), theUMLPackage.getMessage(), null, "transfer_terminate_step", null, 1, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getdatatransfer_Base_Interaction(), theUMLPackage.getInteraction(), null, "base_Interaction", null, 0, 1, datatransfer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(providerEClass, provider.class, "provider", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getprovider_Base_Lifeline(), theUMLPackage.getLifeline(), null, "base_Lifeline", null, 0, 1, provider.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(consumerEClass, consumer.class, "consumer", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getconsumer_Base_Lifeline(), theUMLPackage.getLifeline(), null, "base_Lifeline", null, 0, 1, consumer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

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
		  (idSconnectorEClass,
		   source,
		   new String[] {
			   "originalName", "IDS connector"
		   });
		addAnnotation
		  (datatransferEClass,
		   source,
		   new String[] {
			   "originalName", "data transfer"
		   });
	}

} //Extension4idsPackageImpl
