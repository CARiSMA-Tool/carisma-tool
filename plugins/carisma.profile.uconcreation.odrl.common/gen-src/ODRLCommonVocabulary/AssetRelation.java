/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Asset Relation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.AssetRelation#getType <em>Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.AssetRelation#getAsset <em>Asset</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAssetRelation()
 * @model
 * @generated
 */
public interface AssetRelation extends EObject {
	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.AssetRelationType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see ODRLCommonVocabulary.AssetRelationType
	 * @see #setType(AssetRelationType)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAssetRelation_Type()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	AssetRelationType getType();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.AssetRelation#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see ODRLCommonVocabulary.AssetRelationType
	 * @see #getType()
	 * @generated
	 */
	void setType(AssetRelationType value);

	/**
	 * Returns the value of the '<em><b>Asset</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Asset</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Asset</em>' reference.
	 * @see #setAsset(Asset)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAssetRelation_Asset()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Asset getAsset();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.AssetRelation#getAsset <em>Asset</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Asset</em>' reference.
	 * @see #getAsset()
	 * @generated
	 */
	void setAsset(Asset value);

} // AssetRelation
