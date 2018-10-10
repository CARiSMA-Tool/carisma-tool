/**
 */
package carisma.profile.umlsec.rabac.impl;

import carisma.profile.umlsec.rabac.RabacFactory;
import carisma.profile.umlsec.rabac.RabacPackage;
import carisma.profile.umlsec.rabac.rabac;
import carisma.profile.umlsec.rabac.rabacAttribute;
import carisma.profile.umlsec.rabac.rabacRequire;

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
public class RabacPackageImpl extends EPackageImpl implements RabacPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass rabacEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass rabacAttributeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass rabacRequireEClass = null;

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
	 * @see carisma.profile.umlsec.rabac.RabacPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private RabacPackageImpl() {
		super(eNS_URI, RabacFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link RabacPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static RabacPackage init() {
		if (isInited) return (RabacPackage)EPackage.Registry.INSTANCE.getEPackage(RabacPackage.eNS_URI);

		// Obtain or create and register package
		RabacPackageImpl theRabacPackage = (RabacPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof RabacPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new RabacPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theRabacPackage.createPackageContents();

		// Initialize created meta-data
		theRabacPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theRabacPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(RabacPackage.eNS_URI, theRabacPackage);
		return theRabacPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getrabac() {
		return rabacEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabac_Roles() {
		return (EAttribute)rabacEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabac_Rights() {
		return (EAttribute)rabacEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabac_Rh() {
		return (EAttribute)rabacEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabac_Ssd() {
		return (EAttribute)rabacEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrabac_Base_Class() {
		return (EReference)rabacEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabac_Dsd() {
		return (EAttribute)rabacEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabac_AttributeFilters() {
		return (EAttribute)rabacEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrabac_Base_Package() {
		return (EReference)rabacEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getrabacAttribute() {
		return rabacAttributeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabacAttribute_Name() {
		return (EAttribute)rabacAttributeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrabacAttribute_Base_Operation() {
		return (EReference)rabacAttributeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getrabacRequire() {
		return rabacRequireEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabacRequire_Right() {
		return (EAttribute)rabacRequireEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getrabacRequire_Filters() {
		return (EAttribute)rabacRequireEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrabacRequire_Base_Transition() {
		return (EReference)rabacRequireEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getrabacRequire_Base_Operation() {
		return (EReference)rabacRequireEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RabacFactory getRabacFactory() {
		return (RabacFactory)getEFactoryInstance();
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
		rabacEClass = createEClass(RABAC);
		createEAttribute(rabacEClass, RABAC__ROLES);
		createEAttribute(rabacEClass, RABAC__RIGHTS);
		createEAttribute(rabacEClass, RABAC__RH);
		createEAttribute(rabacEClass, RABAC__SSD);
		createEReference(rabacEClass, RABAC__BASE_CLASS);
		createEAttribute(rabacEClass, RABAC__DSD);
		createEAttribute(rabacEClass, RABAC__ATTRIBUTE_FILTERS);
		createEReference(rabacEClass, RABAC__BASE_PACKAGE);

		rabacAttributeEClass = createEClass(RABAC_ATTRIBUTE);
		createEAttribute(rabacAttributeEClass, RABAC_ATTRIBUTE__NAME);
		createEReference(rabacAttributeEClass, RABAC_ATTRIBUTE__BASE_OPERATION);

		rabacRequireEClass = createEClass(RABAC_REQUIRE);
		createEAttribute(rabacRequireEClass, RABAC_REQUIRE__RIGHT);
		createEAttribute(rabacRequireEClass, RABAC_REQUIRE__FILTERS);
		createEReference(rabacRequireEClass, RABAC_REQUIRE__BASE_TRANSITION);
		createEReference(rabacRequireEClass, RABAC_REQUIRE__BASE_OPERATION);
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
		initEClass(rabacEClass, rabac.class, "rabac", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getrabac_Roles(), theTypesPackage.getString(), "roles", null, 1, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrabac_Rights(), theTypesPackage.getString(), "rights", null, 1, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrabac_Rh(), theTypesPackage.getString(), "rh", null, 0, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrabac_Ssd(), theTypesPackage.getString(), "ssd", null, 0, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrabac_Base_Class(), theUMLPackage.getClass_(), null, "base_Class", null, 1, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrabac_Dsd(), theTypesPackage.getString(), "dsd", null, 0, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrabac_AttributeFilters(), theTypesPackage.getString(), "attributeFilters", null, 0, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrabac_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 1, 1, rabac.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(rabacAttributeEClass, rabacAttribute.class, "rabacAttribute", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getrabacAttribute_Name(), theTypesPackage.getString(), "name", null, 0, 1, rabacAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrabacAttribute_Base_Operation(), theUMLPackage.getOperation(), null, "base_Operation", null, 1, 1, rabacAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(rabacRequireEClass, rabacRequire.class, "rabacRequire", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getrabacRequire_Right(), theTypesPackage.getString(), "right", null, 1, 1, rabacRequire.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getrabacRequire_Filters(), theTypesPackage.getString(), "filters", null, 0, 1, rabacRequire.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, !IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrabacRequire_Base_Transition(), theUMLPackage.getTransition(), null, "base_Transition", null, 1, 1, rabacRequire.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getrabacRequire_Base_Operation(), theUMLPackage.getOperation(), null, "base_Operation", null, 1, 1, rabacRequire.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

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
			 "originalName", "RABAC"
		   });
	}

} //RabacPackageImpl
