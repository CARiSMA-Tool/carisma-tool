/**
 */
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage
 * @generated
 */
public interface Umlsec4idsFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	Umlsec4idsFactory eINSTANCE = carisma.profile.umlsec.umlsec4ids.impl.Umlsec4idsFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>basefree</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>basefree</em>'.
	 * @generated
	 */
	basefree createbasefree();

	/**
	 * Returns a new object of class '<em>base</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>base</em>'.
	 * @generated
	 */
	base createbase();

	/**
	 * Returns a new object of class '<em>trust</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>trust</em>'.
	 * @generated
	 */
	trust createtrust();

	/**
	 * Returns a new object of class '<em>trustplus</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>trustplus</em>'.
	 * @generated
	 */
	trustplus createtrustplus();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	Umlsec4idsPackage getUmlsec4idsPackage();

} //Umlsec4idsFactory
