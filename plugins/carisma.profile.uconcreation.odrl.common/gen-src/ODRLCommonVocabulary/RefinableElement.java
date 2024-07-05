/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Refinable Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * Statt Vererbung der Attribute an bestimmte Stereotypen direkte Anwendung im Diagramm ?
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.RefinableElement#getRefinement <em>Refinement</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRefinableElement()
 * @model abstract="true"
 * @generated
 */
public interface RefinableElement extends EObject {
	/**
	 * Returns the value of the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Refinement</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Refinement</em>' containment reference.
	 * @see #setRefinement(LogicalConstraint)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getRefinableElement_Refinement()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	LogicalConstraint getRefinement();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.RefinableElement#getRefinement <em>Refinement</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Refinement</em>' containment reference.
	 * @see #getRefinement()
	 * @generated
	 */
	void setRefinement(LogicalConstraint value);

} // RefinableElement
