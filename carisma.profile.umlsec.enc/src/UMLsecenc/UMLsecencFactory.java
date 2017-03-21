/**
 */
package UMLsecenc;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see UMLsecenc.UMLsecencPackage
 * @generated
 */
public interface UMLsecencFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	UMLsecencFactory eINSTANCE = UMLsecenc.impl.UMLsecencFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>securelinksenc</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>securelinksenc</em>'.
	 * @generated
	 */
	securelinksenc createsecurelinksenc();

	/**
	 * Returns a new object of class '<em>secrecyenc</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>secrecyenc</em>'.
	 * @generated
	 */
	secrecyenc createsecrecyenc();

	/**
	 * Returns a new object of class '<em>encryptedenc</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>encryptedenc</em>'.
	 * @generated
	 */
	encryptedenc createencryptedenc();

	/**
	 * Returns a new object of class '<em>encryptedpersistence</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>encryptedpersistence</em>'.
	 * @generated
	 */
	encryptedpersistence createencryptedpersistence();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	UMLsecencPackage getUMLsecencPackage();

} //UMLsecencFactory
