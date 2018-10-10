/**
 */
package carisma.profile.umlsec.enc;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.enc.EncPackage
 * @generated
 */
public interface EncFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	EncFactory eINSTANCE = carisma.profile.umlsec.enc.impl.EncFactoryImpl.init();

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
	EncPackage getEncPackage();

} //EncFactory
