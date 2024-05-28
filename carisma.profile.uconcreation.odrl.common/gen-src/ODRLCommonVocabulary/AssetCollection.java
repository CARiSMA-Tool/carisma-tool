/**
 */
package ODRLCommonVocabulary;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Asset Collection</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.AssetCollection#getSource <em>Source</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAssetCollection()
 * @model
 * @generated
 */
public interface AssetCollection extends Asset, RefinableElement {
	/**
	 * Returns the value of the '<em><b>Source</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Source</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Source</em>' attribute.
	 * @see #setSource(String)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAssetCollection_Source()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getSource();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.AssetCollection#getSource <em>Source</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Source</em>' attribute.
	 * @see #getSource()
	 * @generated
	 */
	void setSource(String value);

} // AssetCollection
