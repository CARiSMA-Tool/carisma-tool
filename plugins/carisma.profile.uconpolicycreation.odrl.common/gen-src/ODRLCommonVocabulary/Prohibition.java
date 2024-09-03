/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Prohibition</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.Prohibition#getRemedies <em>Remedies</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getProhibition()
 * @model
 * @generated
 */
public interface Prohibition extends Rule {
	/**
	 * Returns the value of the '<em><b>Remedies</b></em>' reference list.
	 * The list contents are of type {@link ODRLCommonVocabulary.Duty}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Remedies</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Remedies</em>' reference list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getProhibition_Remedies()
	 * @model ordered="false"
	 * @generated
	 */
	EList<Duty> getRemedies();

} // Prohibition
