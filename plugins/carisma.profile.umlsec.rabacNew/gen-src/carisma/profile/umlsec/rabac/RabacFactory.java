/**
 */
package carisma.profile.umlsec.rabac;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.rabac.RabacPackage
 * @generated
 */
public interface RabacFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	RabacFactory eINSTANCE = carisma.profile.umlsec.rabac.impl.RabacFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>rabac</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>rabac</em>'.
	 * @generated
	 */
	rabac createrabac();

	/**
	 * Returns a new object of class '<em>rabac Attribute</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>rabac Attribute</em>'.
	 * @generated
	 */
	rabacAttribute createrabacAttribute();

	/**
	 * Returns a new object of class '<em>rabac Require</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>rabac Require</em>'.
	 * @generated
	 */
	rabacRequire createrabacRequire();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	RabacPackage getRabacPackage();

} //RabacFactory
