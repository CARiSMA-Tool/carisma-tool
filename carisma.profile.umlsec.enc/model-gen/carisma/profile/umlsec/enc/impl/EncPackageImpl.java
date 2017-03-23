/**
 */
package carisma.profile.umlsec.enc.impl;

import carisma.profile.umlsec.UmlsecPackage;

import carisma.profile.umlsec.enc.EncFactory;
import carisma.profile.umlsec.enc.EncPackage;
import carisma.profile.umlsec.enc.encryptedenc;
import carisma.profile.umlsec.enc.encryptedpersistence;
import carisma.profile.umlsec.enc.secrecyenc;
import carisma.profile.umlsec.enc.securelinksenc;

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
public class EncPackageImpl extends EPackageImpl implements EncPackage {
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
	 * @see carisma.profile.umlsec.enc.EncPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private EncPackageImpl() {
		super(eNS_URI, EncFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link EncPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static EncPackage init() {
		if (isInited) return (EncPackage)EPackage.Registry.INSTANCE.getEPackage(EncPackage.eNS_URI);

		// Obtain or create and register package
		EncPackageImpl theEncPackage = (EncPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof EncPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new EncPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		UmlsecPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theEncPackage.createPackageContents();

		// Initialize created meta-data
		theEncPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theEncPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(EncPackage.eNS_URI, theEncPackage);
		return theEncPackage;
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
	public EAttribute getsecurelinksenc_EncAdversary() {
		return (EAttribute)securelinksencEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getsecurelinksenc_EncModel() {
		return (EAttribute)securelinksencEClass.getEStructuralFeatures().get(1);
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
	public EncFactory getEncFactory() {
		return (EncFactory)getEFactoryInstance();
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
		createEAttribute(securelinksencEClass, SECURELINKSENC__ENC_ADVERSARY);
		createEAttribute(securelinksencEClass, SECURELINKSENC__ENC_MODEL);

		secrecyencEClass = createEClass(SECRECYENC);
		createEAttribute(secrecyencEClass, SECRECYENC__TIME);

		encryptedencEClass = createEClass(ENCRYPTEDENC);
		createEAttribute(encryptedencEClass, ENCRYPTEDENC__ALG);
		createEAttribute(encryptedencEClass, ENCRYPTEDENC__KEYLENGTH);

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
		UmlsecPackage theUmlsecPackage = (UmlsecPackage)EPackage.Registry.INSTANCE.getEPackage(UmlsecPackage.eNS_URI);
		TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);
		UMLPackage theUMLPackage = (UMLPackage)EPackage.Registry.INSTANCE.getEPackage(UMLPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		securelinksencEClass.getESuperTypes().add(theUmlsecPackage.getsecurelinks());
		secrecyencEClass.getESuperTypes().add(theUmlsecPackage.getsecrecy());
		encryptedencEClass.getESuperTypes().add(theUmlsecPackage.getencrypted());

		// Initialize classes, features, and operations; add parameters
		initEClass(securelinksencEClass, securelinksenc.class, "securelinksenc", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getsecurelinksenc_EncAdversary(), theTypesPackage.getString(), "encAdversary", null, 1, 1, securelinksenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getsecurelinksenc_EncModel(), theTypesPackage.getString(), "encModel", null, 1, 1, securelinksenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(secrecyencEClass, secrecyenc.class, "secrecyenc", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getsecrecyenc_Time(), theTypesPackage.getString(), "time", null, 1, 1, secrecyenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(encryptedencEClass, encryptedenc.class, "encryptedenc", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getencryptedenc_Alg(), theTypesPackage.getString(), "alg", null, 1, 1, encryptedenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getencryptedenc_Keylength(), theTypesPackage.getString(), "keylength", null, 1, 1, encryptedenc.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

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
		  (this, 
		   source, 
		   new String[] {
			 "originalName", "UMLsecenc"
		   });	
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

} //EncPackageImpl
