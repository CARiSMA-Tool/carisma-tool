/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Constrainable Element</b></em>'.
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
 *   <li>{@link ODRLCommonVocabulary.ConstrainableElement#getConstraint <em>Constraint</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstrainableElement()
 * @model abstract="true"
 * @generated
 */
public interface ConstrainableElement extends EObject {
	/**
	 * Returns the value of the '<em><b>Constraint</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Constraint</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Constraint</em>' containment reference.
	 * @see #setConstraint(LogicalConstraint)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstrainableElement_Constraint()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	LogicalConstraint getConstraint();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.ConstrainableElement#getConstraint <em>Constraint</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Constraint</em>' containment reference.
	 * @see #getConstraint()
	 * @generated
	 */
	void setConstraint(LogicalConstraint value);

} // ConstrainableElement
