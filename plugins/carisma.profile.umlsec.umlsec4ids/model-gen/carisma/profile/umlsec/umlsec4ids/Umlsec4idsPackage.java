/**
 */
package carisma.profile.umlsec.umlsec4ids;

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
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsFactory
 * @model kind="package"
 *        annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='umlsec4ids'"
 * @generated
 */
public interface Umlsec4idsPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "umlsec4ids";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.umlsec.de/profiles/UMLsec/umlsec4ids";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "umlsec4ids";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	Umlsec4idsPackage eINSTANCE = carisma.profile.umlsec.umlsec4ids.impl.Umlsec4idsPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.umlsec4ids.impl.basefreeImpl <em>basefree</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.umlsec4ids.impl.basefreeImpl
	 * @see carisma.profile.umlsec.umlsec4ids.impl.Umlsec4idsPackageImpl#getbasefree()
	 * @generated
	 */
	int BASEFREE = 0;

	/**
	 * The feature id for the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BASEFREE__BASE_NODE = 0;

	/**
	 * The number of structural features of the '<em>basefree</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BASEFREE_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>basefree</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BASEFREE_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.umlsec4ids.basefree <em>basefree</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>basefree</em>'.
	 * @see carisma.profile.umlsec.umlsec4ids.basefree
	 * @generated
	 */
	EClass getbasefree();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.umlsec4ids.basefree#getBase_Node <em>Base Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Node</em>'.
	 * @see carisma.profile.umlsec.umlsec4ids.basefree#getBase_Node()
	 * @see #getbasefree()
	 * @generated
	 */
	EReference getbasefree_Base_Node();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	Umlsec4idsFactory getUmlsec4idsFactory();

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
		 * The meta object literal for the '{@link carisma.profile.umlsec.umlsec4ids.impl.basefreeImpl <em>basefree</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.umlsec4ids.impl.basefreeImpl
		 * @see carisma.profile.umlsec.umlsec4ids.impl.Umlsec4idsPackageImpl#getbasefree()
		 * @generated
		 */
		EClass BASEFREE = eINSTANCE.getbasefree();

		/**
		 * The meta object literal for the '<em><b>Base Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference BASEFREE__BASE_NODE = eINSTANCE.getbasefree_Base_Node();

	}

} //Umlsec4idsPackage
