/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Permission</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.Permission#getDuties <em>Duties</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPermission()
 * @model
 * @generated
 */
public interface Permission extends Rule {
	/**
	 * Returns the value of the '<em><b>Duties</b></em>' reference list.
	 * The list contents are of type {@link ODRLCommonVocabulary.Duty}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Duties</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Duties</em>' reference list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPermission_Duties()
	 * @model ordered="false"
	 * @generated
	 */
	EList<Duty> getDuties();

} // Permission
