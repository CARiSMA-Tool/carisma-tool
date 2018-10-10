/**
 */
package carisma.profile.umlsec.rabac;

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
 * @see carisma.profile.umlsec.rabac.RabacFactory
 * @model kind="package"
 *        annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='RABAC'"
 * @generated
 */
public interface RabacPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "rabac";

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
	RabacPackage eINSTANCE = carisma.profile.umlsec.rabac.impl.RabacPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.rabac.impl.rabacImpl <em>rabac</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.rabac.impl.rabacImpl
	 * @see carisma.profile.umlsec.rabac.impl.RabacPackageImpl#getrabac()
	 * @generated
	 */
	int RABAC = 0;

	/**
	 * The feature id for the '<em><b>Roles</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__ROLES = 0;

	/**
	 * The feature id for the '<em><b>Rights</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__RIGHTS = 1;

	/**
	 * The feature id for the '<em><b>Rh</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__RH = 2;

	/**
	 * The feature id for the '<em><b>Ssd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__SSD = 3;

	/**
	 * The feature id for the '<em><b>Base Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__BASE_CLASS = 4;

	/**
	 * The feature id for the '<em><b>Dsd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__DSD = 5;

	/**
	 * The feature id for the '<em><b>Attribute Filters</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__ATTRIBUTE_FILTERS = 6;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC__BASE_PACKAGE = 7;

	/**
	 * The number of structural features of the '<em>rabac</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_FEATURE_COUNT = 8;

	/**
	 * The number of operations of the '<em>rabac</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.rabac.impl.rabacAttributeImpl <em>rabac Attribute</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.rabac.impl.rabacAttributeImpl
	 * @see carisma.profile.umlsec.rabac.impl.RabacPackageImpl#getrabacAttribute()
	 * @generated
	 */
	int RABAC_ATTRIBUTE = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_ATTRIBUTE__NAME = 0;

	/**
	 * The feature id for the '<em><b>Base Operation</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_ATTRIBUTE__BASE_OPERATION = 1;

	/**
	 * The number of structural features of the '<em>rabac Attribute</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_ATTRIBUTE_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>rabac Attribute</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_ATTRIBUTE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.rabac.impl.rabacRequireImpl <em>rabac Require</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.rabac.impl.rabacRequireImpl
	 * @see carisma.profile.umlsec.rabac.impl.RabacPackageImpl#getrabacRequire()
	 * @generated
	 */
	int RABAC_REQUIRE = 2;

	/**
	 * The feature id for the '<em><b>Right</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_REQUIRE__RIGHT = 0;

	/**
	 * The feature id for the '<em><b>Filters</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_REQUIRE__FILTERS = 1;

	/**
	 * The feature id for the '<em><b>Base Transition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_REQUIRE__BASE_TRANSITION = 2;

	/**
	 * The feature id for the '<em><b>Base Operation</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_REQUIRE__BASE_OPERATION = 3;

	/**
	 * The number of structural features of the '<em>rabac Require</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_REQUIRE_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>rabac Require</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RABAC_REQUIRE_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.rabac.rabac <em>rabac</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>rabac</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac
	 * @generated
	 */
	EClass getrabac();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabac#getRoles <em>Roles</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Roles</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getRoles()
	 * @see #getrabac()
	 * @generated
	 */
	EAttribute getrabac_Roles();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabac#getRights <em>Rights</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Rights</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getRights()
	 * @see #getrabac()
	 * @generated
	 */
	EAttribute getrabac_Rights();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabac#getRh <em>Rh</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Rh</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getRh()
	 * @see #getrabac()
	 * @generated
	 */
	EAttribute getrabac_Rh();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabac#getSsd <em>Ssd</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ssd</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getSsd()
	 * @see #getrabac()
	 * @generated
	 */
	EAttribute getrabac_Ssd();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.rabac.rabac#getBase_Class <em>Base Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Class</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getBase_Class()
	 * @see #getrabac()
	 * @generated
	 */
	EReference getrabac_Base_Class();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabac#getDsd <em>Dsd</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Dsd</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getDsd()
	 * @see #getrabac()
	 * @generated
	 */
	EAttribute getrabac_Dsd();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabac#getAttributeFilters <em>Attribute Filters</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Attribute Filters</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getAttributeFilters()
	 * @see #getrabac()
	 * @generated
	 */
	EAttribute getrabac_AttributeFilters();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.rabac.rabac#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see carisma.profile.umlsec.rabac.rabac#getBase_Package()
	 * @see #getrabac()
	 * @generated
	 */
	EReference getrabac_Base_Package();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.rabac.rabacAttribute <em>rabac Attribute</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>rabac Attribute</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacAttribute
	 * @generated
	 */
	EClass getrabacAttribute();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabacAttribute#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacAttribute#getName()
	 * @see #getrabacAttribute()
	 * @generated
	 */
	EAttribute getrabacAttribute_Name();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.rabac.rabacAttribute#getBase_Operation <em>Base Operation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Operation</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacAttribute#getBase_Operation()
	 * @see #getrabacAttribute()
	 * @generated
	 */
	EReference getrabacAttribute_Base_Operation();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.rabac.rabacRequire <em>rabac Require</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>rabac Require</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacRequire
	 * @generated
	 */
	EClass getrabacRequire();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabacRequire#getRight <em>Right</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Right</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacRequire#getRight()
	 * @see #getrabacRequire()
	 * @generated
	 */
	EAttribute getrabacRequire_Right();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.rabac.rabacRequire#getFilters <em>Filters</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Filters</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacRequire#getFilters()
	 * @see #getrabacRequire()
	 * @generated
	 */
	EAttribute getrabacRequire_Filters();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.rabac.rabacRequire#getBase_Transition <em>Base Transition</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Transition</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacRequire#getBase_Transition()
	 * @see #getrabacRequire()
	 * @generated
	 */
	EReference getrabacRequire_Base_Transition();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.rabac.rabacRequire#getBase_Operation <em>Base Operation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Operation</em>'.
	 * @see carisma.profile.umlsec.rabac.rabacRequire#getBase_Operation()
	 * @see #getrabacRequire()
	 * @generated
	 */
	EReference getrabacRequire_Base_Operation();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	RabacFactory getRabacFactory();

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
		 * The meta object literal for the '{@link carisma.profile.umlsec.rabac.impl.rabacImpl <em>rabac</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.rabac.impl.rabacImpl
		 * @see carisma.profile.umlsec.rabac.impl.RabacPackageImpl#getrabac()
		 * @generated
		 */
		EClass RABAC = eINSTANCE.getrabac();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC__ROLES = eINSTANCE.getrabac_Roles();

		/**
		 * The meta object literal for the '<em><b>Rights</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC__RIGHTS = eINSTANCE.getrabac_Rights();

		/**
		 * The meta object literal for the '<em><b>Rh</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC__RH = eINSTANCE.getrabac_Rh();

		/**
		 * The meta object literal for the '<em><b>Ssd</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC__SSD = eINSTANCE.getrabac_Ssd();

		/**
		 * The meta object literal for the '<em><b>Base Class</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RABAC__BASE_CLASS = eINSTANCE.getrabac_Base_Class();

		/**
		 * The meta object literal for the '<em><b>Dsd</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC__DSD = eINSTANCE.getrabac_Dsd();

		/**
		 * The meta object literal for the '<em><b>Attribute Filters</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC__ATTRIBUTE_FILTERS = eINSTANCE.getrabac_AttributeFilters();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RABAC__BASE_PACKAGE = eINSTANCE.getrabac_Base_Package();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.rabac.impl.rabacAttributeImpl <em>rabac Attribute</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.rabac.impl.rabacAttributeImpl
		 * @see carisma.profile.umlsec.rabac.impl.RabacPackageImpl#getrabacAttribute()
		 * @generated
		 */
		EClass RABAC_ATTRIBUTE = eINSTANCE.getrabacAttribute();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC_ATTRIBUTE__NAME = eINSTANCE.getrabacAttribute_Name();

		/**
		 * The meta object literal for the '<em><b>Base Operation</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RABAC_ATTRIBUTE__BASE_OPERATION = eINSTANCE.getrabacAttribute_Base_Operation();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.rabac.impl.rabacRequireImpl <em>rabac Require</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.rabac.impl.rabacRequireImpl
		 * @see carisma.profile.umlsec.rabac.impl.RabacPackageImpl#getrabacRequire()
		 * @generated
		 */
		EClass RABAC_REQUIRE = eINSTANCE.getrabacRequire();

		/**
		 * The meta object literal for the '<em><b>Right</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC_REQUIRE__RIGHT = eINSTANCE.getrabacRequire_Right();

		/**
		 * The meta object literal for the '<em><b>Filters</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RABAC_REQUIRE__FILTERS = eINSTANCE.getrabacRequire_Filters();

		/**
		 * The meta object literal for the '<em><b>Base Transition</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RABAC_REQUIRE__BASE_TRANSITION = eINSTANCE.getrabacRequire_Base_Transition();

		/**
		 * The meta object literal for the '<em><b>Base Operation</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RABAC_REQUIRE__BASE_OPERATION = eINSTANCE.getrabacRequire_Base_Operation();

	}

} //RabacPackage
