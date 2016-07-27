/**
 */
package RABAC;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see RABAC.RABACPackage
 * @generated
 */
public interface RABACFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	RABACFactory eINSTANCE = RABAC.impl.RABACFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>abac</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>abac</em>'.
	 * @generated
	 */
	abac createabac();

	/**
	 * Returns a new object of class '<em>abac Attribute</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>abac Attribute</em>'.
	 * @generated
	 */
	abacAttribute createabacAttribute();

	/**
	 * Returns a new object of class '<em>abac Require</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>abac Require</em>'.
	 * @generated
	 */
	abacRequire createabacRequire();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	RABACPackage getRABACPackage();

} //RABACFactory
