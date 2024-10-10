/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage
 * @generated
 */
public interface ODRLCommonVocabularyFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ODRLCommonVocabularyFactory eINSTANCE = ODRLCommonVocabulary.impl.ODRLCommonVocabularyFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>ODRL Policy</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>ODRL Policy</em>'.
	 * @generated
	 */
	ODRLPolicy createODRLPolicy();

	/**
	 * Returns a new object of class '<em>Logical Constraint</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Logical Constraint</em>'.
	 * @generated
	 */
	LogicalConstraint createLogicalConstraint();

	/**
	 * Returns a new object of class '<em>Constraint</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Constraint</em>'.
	 * @generated
	 */
	Constraint createConstraint();

	/**
	 * Returns a new object of class '<em>Party Function</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Party Function</em>'.
	 * @generated
	 */
	PartyFunction createPartyFunction();

	/**
	 * Returns a new object of class '<em>Party</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Party</em>'.
	 * @generated
	 */
	Party createParty();

	/**
	 * Returns a new object of class '<em>Permission</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Permission</em>'.
	 * @generated
	 */
	Permission createPermission();

	/**
	 * Returns a new object of class '<em>Duty</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Duty</em>'.
	 * @generated
	 */
	Duty createDuty();

	/**
	 * Returns a new object of class '<em>Prohibition</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Prohibition</em>'.
	 * @generated
	 */
	Prohibition createProhibition();

	/**
	 * Returns a new object of class '<em>Asset</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Asset</em>'.
	 * @generated
	 */
	Asset createAsset();

	/**
	 * Returns a new object of class '<em>Asset Collection</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Asset Collection</em>'.
	 * @generated
	 */
	AssetCollection createAssetCollection();

	/**
	 * Returns a new object of class '<em>Party Collection</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Party Collection</em>'.
	 * @generated
	 */
	PartyCollection createPartyCollection();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	ODRLCommonVocabularyPackage getODRLCommonVocabularyPackage();

} //ODRLCommonVocabularyFactory
