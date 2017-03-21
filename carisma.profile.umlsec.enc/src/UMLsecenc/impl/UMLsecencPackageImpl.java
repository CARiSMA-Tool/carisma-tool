/**
 */
package UMLsecenc.impl;

import UMLsecenc.UMLsecencFactory;
import UMLsecenc.UMLsecencPackage;
import UMLsecenc.encryptedenc;
import UMLsecenc.encryptedpersistence;
import UMLsecenc.secrecyenc;
import UMLsecenc.securelinksenc;

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
public class UMLsecencPackageImpl extends EPackageImpl implements UMLsecencPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass securelinksencEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass secrecyencEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass encryptedencEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass encryptedpersistenceEClass = null;

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
	 * @see UMLsecenc.UMLsecencPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private UMLsecencPackageImpl() {
		super(eNS_URI, UMLsecencFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link UMLsecencPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static UMLsecencPackage init() {
		if (isInited) return (UMLsecencPackage)EPackage.Registry.INSTANCE.getEPackage(UMLsecencPackage.eNS_URI);

		// Obtain or create and register package
		UMLsecencPackageImpl theUMLsecencPackage = (UMLsecencPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof UMLsecencPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new UMLsecencPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theUMLsecencPackage.createPackageContents();

		// Initialize created meta-data
		theUMLsecencPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theUMLsecencPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(UMLsecencPackage.eNS_URI, theUMLsecencPackage);
		return theUMLsecencPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsecurelinksenc() {
		return securelinksencEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getsecurelinksenc_Adversary() {
		return (EAttribute)securelinksencEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getsecurelinksenc_EncAdversary() {
		return (EAttribute)securelinksencEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getsecurelinksenc_EncModel() {
		return (EAttribute)securelinksencEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsecurelinksenc_Base_Package() {
		return (EReference)securelinksencEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getsecrecyenc() {
		return secrecyencEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getsecrecyenc_Time() {
		return (EAttribute)secrecyencEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getsecrecyenc_Base_Dependency() {
		return (EReference)secrecyencEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getencryptedenc() {
		return encryptedencEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getencryptedenc_Alg() {
		return (EAttribute)encryptedencEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getencryptedenc_Keylength() {
		return (EAttribute)encryptedencEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getencryptedenc_Base_CommunicationPath() {
		return (EReference)encryptedencEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getencryptedpersistence() {
		return encryptedpersistenceEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getencryptedpersistence_Base_Class() {
		return (EReference)encryptedpersistenceEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getencryptedpersistence_Alg() {
		return (EAttribute)encryptedpersistenceEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getencryptedpersistence_Keylength() {
		return (EAttribute)encryptedpersistenceEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UMLsecencFactory getUMLsecencFactory() {
		return (UMLsecencFactory)getEFactoryInstance();
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
		securelinksencEClass = createEClass(SECURELINKSENC);
		createEAttribute(securelinksencEClass, SECURELINKSENC__ADVERSARY);
		createEAttribute(securelinksencEClass, SECURELINKSENC__ENC_ADVERSARY);
		createEAttribute(securelinksencEClass, SECURELINKSENC__ENC_MODEL);
		createEReference(securelinksencEClass, SECURELINKSENC__BASE_PACKAGE);

		secrecyencEClass = createEClass(SECRECYENC);
		createEAttribute(secrecyencEClass, SECRECYENC__TIME);
		createEReference(secrecyencEClass, SECRECYENC__BASE_DEPENDENCY);

		encryptedencEClass = createEClass(ENCRYPTEDENC);
		createEAttribute(encryptedencEClass, ENCRYPTEDENC__ALG);
		createEAttribute(encryptedencEClass, ENCRYPTEDENC__KEYLENGTH);
		createEReference(encryptedencEClass, ENCRYPTEDENC__BASE_COMMUNICATION_PATH);

		encryptedpersistenceEClass = createEClass(ENCRYPTEDPERSISTENCE);
		createEReference(encryptedpersistenceEClass, ENCRYPTEDPERSISTENCE__BASE_CLASS);
		createEAttribute(encryptedpersistenceEClass, ENCRYPTEDPERSISTENCE__ALG);
		createEAttribute(encryptedpersistenceEClass, ENCRYPTEDPERSISTENCE__KEYLENGTH);
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
		initEClass(securelinksencEClass, securelinksenc.class, "securelinksenc", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getsecurelinksenc_Adversary(), theTypesPackage.getString(), "adversary", null, 1, 1, securelinksenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getsecurelinksenc_EncAdversary(), theTypesPackage.getString(), "encAdversary", null, 1, 1, securelinksenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getsecurelinksenc_EncModel(), theTypesPackage.getString(), "encModel", null, 1, 1, securelinksenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getsecurelinksenc_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, securelinksenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(secrecyencEClass, secrecyenc.class, "secrecyenc", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getsecrecyenc_Time(), theTypesPackage.getString(), "time", null, 1, 1, secrecyenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getsecrecyenc_Base_Dependency(), theUMLPackage.getDependency(), null, "base_Dependency", null, 1, 1, secrecyenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(encryptedencEClass, encryptedenc.class, "encryptedenc", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getencryptedenc_Alg(), theTypesPackage.getString(), "alg", null, 1, 1, encryptedenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getencryptedenc_Keylength(), theTypesPackage.getString(), "keylength", null, 1, 1, encryptedenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getencryptedenc_Base_CommunicationPath(), theUMLPackage.getCommunicationPath(), null, "base_CommunicationPath", null, 1, 1, encryptedenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(encryptedpersistenceEClass, encryptedpersistence.class, "encryptedpersistence", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getencryptedpersistence_Base_Class(), theUMLPackage.getClass_(), null, "base_Class", null, 1, 1, encryptedpersistence.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getencryptedpersistence_Alg(), theTypesPackage.getString(), "alg", null, 1, 1, encryptedpersistence.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getencryptedpersistence_Keylength(), theTypesPackage.getString(), "keylength", null, 1, 1, encryptedpersistence.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

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
		  (securelinksencEClass, 
		   source, 
		   new String[] {
			 "originalName", "secure links enc"
		   });	
		addAnnotation
		  (secrecyencEClass, 
		   source, 
		   new String[] {
			 "originalName", "secrecy enc"
		   });	
		addAnnotation
		  (encryptedencEClass, 
		   source, 
		   new String[] {
			 "originalName", "encrypted enc"
		   });	
		addAnnotation
		  (encryptedpersistenceEClass, 
		   source, 
		   new String[] {
			 "originalName", "encrypted persistence"
		   });
	}

} //UMLsecencPackageImpl
