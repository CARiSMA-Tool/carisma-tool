/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Party Function</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.PartyFunction#getType <em>Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.PartyFunction#getParty <em>Party</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPartyFunction()
 * @model
 * @generated
 */
public interface PartyFunction extends EObject {
	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.PartyFunctionType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see ODRLCommonVocabulary.PartyFunctionType
	 * @see #setType(PartyFunctionType)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPartyFunction_Type()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	PartyFunctionType getType();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.PartyFunction#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see ODRLCommonVocabulary.PartyFunctionType
	 * @see #getType()
	 * @generated
	 */
	void setType(PartyFunctionType value);

	/**
	 * Returns the value of the '<em><b>Party</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Party</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Party</em>' reference.
	 * @see #setParty(Party)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPartyFunction_Party()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Party getParty();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.PartyFunction#getParty <em>Party</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Party</em>' reference.
	 * @see #getParty()
	 * @generated
	 */
	void setParty(Party value);

} // PartyFunction
