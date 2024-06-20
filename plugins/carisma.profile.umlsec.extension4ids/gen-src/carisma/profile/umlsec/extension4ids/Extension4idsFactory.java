/**
 */
package carisma.profile.umlsec.extension4ids;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage
 * @generated
 */
public interface Extension4idsFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	Extension4idsFactory eINSTANCE = carisma.profile.umlsec.extension4ids.impl.Extension4idsFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>ID Sconnector</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>ID Sconnector</em>'.
	 * @generated
	 */
	IDSconnector createIDSconnector();

	/**
	 * Returns a new object of class '<em>usagecontrol</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>usagecontrol</em>'.
	 * @generated
	 */
	usagecontrol createusagecontrol();

	/**
	 * Returns a new object of class '<em>datatransfer</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>datatransfer</em>'.
	 * @generated
	 */
	datatransfer createdatatransfer();

	/**
	 * Returns a new object of class '<em>provider</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>provider</em>'.
	 * @generated
	 */
	provider createprovider();

	/**
	 * Returns a new object of class '<em>consumer</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>consumer</em>'.
	 * @generated
	 */
	consumer createconsumer();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	Extension4idsPackage getExtension4idsPackage();

} //Extension4idsFactory
