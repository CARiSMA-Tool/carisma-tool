/**
 */
package RABAC;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see RABAC.RABACFactory
 * @model kind="package"
 * @generated
 */
public interface RABACPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "RABAC";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.umlsec.de/profiles/UMLsec/RABAC";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "RABAC";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	RABACPackage eINSTANCE = RABAC.impl.RABACPackageImpl.init();

	/**
	 * The meta object id for the '{@link RABAC.impl.abacImpl <em>abac</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see RABAC.impl.abacImpl
	 * @see RABAC.impl.RABACPackageImpl#getabac()
	 * @generated
	 */
	int ABAC = 0;

	/**
	 * The feature id for the '<em><b>Roles</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__ROLES = 0;

	/**
	 * The feature id for the '<em><b>Rights</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__RIGHTS = 1;

	/**
	 * The feature id for the '<em><b>Rh</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__RH = 2;

	/**
	 * The feature id for the '<em><b>Ssd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__SSD = 3;

	/**
	 * The feature id for the '<em><b>Base Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__BASE_CLASS = 4;

	/**
	 * The feature id for the '<em><b>Dsd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__DSD = 5;

	/**
	 * The feature id for the '<em><b>Attribute Filters</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC__ATTRIBUTE_FILTERS = 6;

	/**
	 * The number of structural features of the '<em>abac</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_FEATURE_COUNT = 7;

	/**
	 * The number of operations of the '<em>abac</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link RABAC.impl.abacAttributeImpl <em>abac Attribute</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see RABAC.impl.abacAttributeImpl
	 * @see RABAC.impl.RABACPackageImpl#getabacAttribute()
	 * @generated
	 */
	int ABAC_ATTRIBUTE = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_ATTRIBUTE__NAME = 0;

	/**
	 * The feature id for the '<em><b>Base Operation</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_ATTRIBUTE__BASE_OPERATION = 1;

	/**
	 * The number of structural features of the '<em>abac Attribute</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_ATTRIBUTE_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>abac Attribute</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_ATTRIBUTE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link RABAC.impl.abacRequireImpl <em>abac Require</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see RABAC.impl.abacRequireImpl
	 * @see RABAC.impl.RABACPackageImpl#getabacRequire()
	 * @generated
	 */
	int ABAC_REQUIRE = 2;

	/**
	 * The feature id for the '<em><b>Right</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_REQUIRE__RIGHT = 0;

	/**
	 * The feature id for the '<em><b>Filters</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_REQUIRE__FILTERS = 1;

	/**
	 * The feature id for the '<em><b>Base Transition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_REQUIRE__BASE_TRANSITION = 2;

	/**
	 * The feature id for the '<em><b>Base Operation</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_REQUIRE__BASE_OPERATION = 3;

	/**
	 * The number of structural features of the '<em>abac Require</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_REQUIRE_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>abac Require</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABAC_REQUIRE_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link RABAC.abac <em>abac</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>abac</em>'.
	 * @see RABAC.abac
	 * @generated
	 */
	EClass getabac();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abac#getRoles <em>Roles</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Roles</em>'.
	 * @see RABAC.abac#getRoles()
	 * @see #getabac()
	 * @generated
	 */
	EAttribute getabac_Roles();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abac#getRights <em>Rights</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Rights</em>'.
	 * @see RABAC.abac#getRights()
	 * @see #getabac()
	 * @generated
	 */
	EAttribute getabac_Rights();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abac#getRh <em>Rh</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Rh</em>'.
	 * @see RABAC.abac#getRh()
	 * @see #getabac()
	 * @generated
	 */
	EAttribute getabac_Rh();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abac#getSsd <em>Ssd</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ssd</em>'.
	 * @see RABAC.abac#getSsd()
	 * @see #getabac()
	 * @generated
	 */
	EAttribute getabac_Ssd();

	/**
	 * Returns the meta object for the reference '{@link RABAC.abac#getBase_Class <em>Base Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Class</em>'.
	 * @see RABAC.abac#getBase_Class()
	 * @see #getabac()
	 * @generated
	 */
	EReference getabac_Base_Class();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abac#getDsd <em>Dsd</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Dsd</em>'.
	 * @see RABAC.abac#getDsd()
	 * @see #getabac()
	 * @generated
	 */
	EAttribute getabac_Dsd();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abac#getAttributeFilters <em>Attribute Filters</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Attribute Filters</em>'.
	 * @see RABAC.abac#getAttributeFilters()
	 * @see #getabac()
	 * @generated
	 */
	EAttribute getabac_AttributeFilters();

	/**
	 * Returns the meta object for class '{@link RABAC.abacAttribute <em>abac Attribute</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>abac Attribute</em>'.
	 * @see RABAC.abacAttribute
	 * @generated
	 */
	EClass getabacAttribute();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abacAttribute#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see RABAC.abacAttribute#getName()
	 * @see #getabacAttribute()
	 * @generated
	 */
	EAttribute getabacAttribute_Name();

	/**
	 * Returns the meta object for the reference '{@link RABAC.abacAttribute#getBase_Operation <em>Base Operation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Operation</em>'.
	 * @see RABAC.abacAttribute#getBase_Operation()
	 * @see #getabacAttribute()
	 * @generated
	 */
	EReference getabacAttribute_Base_Operation();

	/**
	 * Returns the meta object for class '{@link RABAC.abacRequire <em>abac Require</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>abac Require</em>'.
	 * @see RABAC.abacRequire
	 * @generated
	 */
	EClass getabacRequire();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abacRequire#getRight <em>Right</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Right</em>'.
	 * @see RABAC.abacRequire#getRight()
	 * @see #getabacRequire()
	 * @generated
	 */
	EAttribute getabacRequire_Right();

	/**
	 * Returns the meta object for the attribute '{@link RABAC.abacRequire#getFilters <em>Filters</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Filters</em>'.
	 * @see RABAC.abacRequire#getFilters()
	 * @see #getabacRequire()
	 * @generated
	 */
	EAttribute getabacRequire_Filters();

	/**
	 * Returns the meta object for the reference '{@link RABAC.abacRequire#getBase_Transition <em>Base Transition</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Transition</em>'.
	 * @see RABAC.abacRequire#getBase_Transition()
	 * @see #getabacRequire()
	 * @generated
	 */
	EReference getabacRequire_Base_Transition();

	/**
	 * Returns the meta object for the reference '{@link RABAC.abacRequire#getBase_Operation <em>Base Operation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Operation</em>'.
	 * @see RABAC.abacRequire#getBase_Operation()
	 * @see #getabacRequire()
	 * @generated
	 */
	EReference getabacRequire_Base_Operation();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	RABACFactory getRABACFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link RABAC.impl.abacImpl <em>abac</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see RABAC.impl.abacImpl
		 * @see RABAC.impl.RABACPackageImpl#getabac()
		 * @generated
		 */
		EClass ABAC = eINSTANCE.getabac();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC__ROLES = eINSTANCE.getabac_Roles();

		/**
		 * The meta object literal for the '<em><b>Rights</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC__RIGHTS = eINSTANCE.getabac_Rights();

		/**
		 * The meta object literal for the '<em><b>Rh</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC__RH = eINSTANCE.getabac_Rh();

		/**
		 * The meta object literal for the '<em><b>Ssd</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC__SSD = eINSTANCE.getabac_Ssd();

		/**
		 * The meta object literal for the '<em><b>Base Class</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ABAC__BASE_CLASS = eINSTANCE.getabac_Base_Class();

		/**
		 * The meta object literal for the '<em><b>Dsd</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC__DSD = eINSTANCE.getabac_Dsd();

		/**
		 * The meta object literal for the '<em><b>Attribute Filters</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC__ATTRIBUTE_FILTERS = eINSTANCE.getabac_AttributeFilters();

		/**
		 * The meta object literal for the '{@link RABAC.impl.abacAttributeImpl <em>abac Attribute</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see RABAC.impl.abacAttributeImpl
		 * @see RABAC.impl.RABACPackageImpl#getabacAttribute()
		 * @generated
		 */
		EClass ABAC_ATTRIBUTE = eINSTANCE.getabacAttribute();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC_ATTRIBUTE__NAME = eINSTANCE.getabacAttribute_Name();

		/**
		 * The meta object literal for the '<em><b>Base Operation</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ABAC_ATTRIBUTE__BASE_OPERATION = eINSTANCE.getabacAttribute_Base_Operation();

		/**
		 * The meta object literal for the '{@link RABAC.impl.abacRequireImpl <em>abac Require</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see RABAC.impl.abacRequireImpl
		 * @see RABAC.impl.RABACPackageImpl#getabacRequire()
		 * @generated
		 */
		EClass ABAC_REQUIRE = eINSTANCE.getabacRequire();

		/**
		 * The meta object literal for the '<em><b>Right</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC_REQUIRE__RIGHT = eINSTANCE.getabacRequire_Right();

		/**
		 * The meta object literal for the '<em><b>Filters</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABAC_REQUIRE__FILTERS = eINSTANCE.getabacRequire_Filters();

		/**
		 * The meta object literal for the '<em><b>Base Transition</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ABAC_REQUIRE__BASE_TRANSITION = eINSTANCE.getabacRequire_Base_Transition();

		/**
		 * The meta object literal for the '<em><b>Base Operation</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ABAC_REQUIRE__BASE_OPERATION = eINSTANCE.getabacRequire_Base_Operation();

	}

} //RABACPackage
