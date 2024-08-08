/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * Die Attribute mit DatyType-Typ haben im Diagramm eine Darstellung, die sehr viel Platz wegnimmt und für einen Betrachter nicht hilfreich ist.
 * Entsprechende Attribute müssen mit einem CSS-Stylesheet versteckt werden.
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.Rule#getBase_Action <em>Base Action</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Rule#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Rule#getInvolvedParties <em>Involved Parties</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Rule#getAction <em>Action</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRule()
 * @model abstract="true"
 * @generated
 */
public interface Rule extends ConstrainableElement, RefinableElement {
	/**
	 * Returns the value of the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Action</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Action</em>' reference.
	 * @see #setBase_Action(Action)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRule_Base_Action()
	 * @model ordered="false"
	 * @generated
	 */
	Action getBase_Action();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Rule#getBase_Action <em>Base Action</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Action</em>' reference.
	 * @see #getBase_Action()
	 * @generated
	 */
	void setBase_Action(Action value);

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
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRule_Uid()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getUid();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Rule#getUid <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Uid</em>' attribute.
	 * @see #getUid()
	 * @generated
	 */
	void setUid(String value);

	/**
	 * Returns the value of the '<em><b>Involved Parties</b></em>' containment reference list.
	 * The list contents are of type {@link ODRLCommonVocabulary.PartyFunction}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Involved Parties</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Involved Parties</em>' containment reference list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRule_InvolvedParties()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	EList<PartyFunction> getInvolvedParties();

	/**
	 * Returns the value of the '<em><b>Action</b></em>' attribute.
	 * The default value is <code>"Null"</code>.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.Action}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Action</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Action</em>' attribute.
	 * @see ODRLCommonVocabulary.Action
	 * @see #setAction(ODRLCommonVocabulary.Action)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRule_Action()
	 * @model default="Null" required="true" ordered="false"
	 * @generated
	 */
	ODRLCommonVocabulary.Action getAction();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Rule#getAction <em>Action</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Action</em>' attribute.
	 * @see ODRLCommonVocabulary.Action
	 * @see #getAction()
	 * @generated
	 */
	void setAction(ODRLCommonVocabulary.Action value);

} // Rule
