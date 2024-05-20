/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Duty</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.Duty#getConsequences <em>Consequences</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getDuty()
 * @model
 * @generated
 */
public interface Duty extends Rule {
	/**
	 * Returns the value of the '<em><b>Consequences</b></em>' reference list.
	 * The list contents are of type {@link ODRLCommonVocabulary.Duty}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Consequences</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Consequences</em>' reference list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getDuty_Consequences()
	 * @model ordered="false"
	 * @generated
	 */
	EList<Duty> getConsequences();

} // Duty
