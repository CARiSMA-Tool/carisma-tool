/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.DataStoreNode;
import org.eclipse.uml2.uml.Pin;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Asset</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.Asset#getBase_DataStoreNode <em>Base Data Store Node</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Asset#getBase_Pin <em>Base Pin</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Asset#getUid <em>Uid</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAsset()
 * @model
 * @generated
 */
public interface Asset extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Data Store Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Data Store Node</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Data Store Node</em>' reference.
	 * @see #setBase_DataStoreNode(DataStoreNode)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAsset_Base_DataStoreNode()
	 * @model ordered="false"
	 * @generated
	 */
	DataStoreNode getBase_DataStoreNode();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Asset#getBase_DataStoreNode <em>Base Data Store Node</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Data Store Node</em>' reference.
	 * @see #getBase_DataStoreNode()
	 * @generated
	 */
	void setBase_DataStoreNode(DataStoreNode value);

	/**
	 * Returns the value of the '<em><b>Base Pin</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Pin</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Pin</em>' reference.
	 * @see #setBase_Pin(Pin)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAsset_Base_Pin()
	 * @model ordered="false"
	 * @generated
	 */
	Pin getBase_Pin();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Asset#getBase_Pin <em>Base Pin</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Pin</em>' reference.
	 * @see #getBase_Pin()
	 * @generated
	 */
	void setBase_Pin(Pin value);

	/**
	 * Returns the value of the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Uid</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Uid</em>' attribute.
	 * @see #setUid(String)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getAsset_Uid()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getUid();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Asset#getUid <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Uid</em>' attribute.
	 * @see #getUid()
	 * @generated
	 */
	void setUid(String value);

} // Asset
