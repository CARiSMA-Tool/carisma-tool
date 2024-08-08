/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Activity;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>ODRL Policy</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * Nach aktuellem Stand wird das gleichzeitige Anwenden von mehreren ODRL-Profilen mit separat erstellten UML-Profilen nicht möglich sein, da die enumerations der Profile nicht zusammengeführt werden können.
 * Profiles-Attribut gegebenenfalls als String in das entsprechende UML-Profil schreiben
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.ODRLPolicy#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.ODRLPolicy#getBase_Activity <em>Base Activity</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.ODRLPolicy#getConflictStrategy <em>Conflict Strategy</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.ODRLPolicy#getPolicyType <em>Policy Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.ODRLPolicy#getProfiles <em>Profiles</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.ODRLPolicy#getInheritsFrom <em>Inherits From</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='ODRL-Policy'"
 * @generated
 */
public interface ODRLPolicy extends EObject {
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
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy_Uid()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getUid();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.ODRLPolicy#getUid <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Uid</em>' attribute.
	 * @see #getUid()
	 * @generated
	 */
	void setUid(String value);

	/**
	 * Returns the value of the '<em><b>Base Activity</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Activity</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Activity</em>' reference.
	 * @see #setBase_Activity(Activity)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy_Base_Activity()
	 * @model ordered="false"
	 * @generated
	 */
	Activity getBase_Activity();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.ODRLPolicy#getBase_Activity <em>Base Activity</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Activity</em>' reference.
	 * @see #getBase_Activity()
	 * @generated
	 */
	void setBase_Activity(Activity value);

	/**
	 * Returns the value of the '<em><b>Conflict Strategy</b></em>' attribute.
	 * The default value is <code>"Null"</code>.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.ConflictStrategy}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Conflict Strategy</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Conflict Strategy</em>' attribute.
	 * @see ODRLCommonVocabulary.ConflictStrategy
	 * @see #setConflictStrategy(ConflictStrategy)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy_ConflictStrategy()
	 * @model default="Null" ordered="false"
	 * @generated
	 */
	ConflictStrategy getConflictStrategy();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.ODRLPolicy#getConflictStrategy <em>Conflict Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Conflict Strategy</em>' attribute.
	 * @see ODRLCommonVocabulary.ConflictStrategy
	 * @see #getConflictStrategy()
	 * @generated
	 */
	void setConflictStrategy(ConflictStrategy value);

	/**
	 * Returns the value of the '<em><b>Policy Type</b></em>' attribute.
	 * The default value is <code>"Null"</code>.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.PolicyType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Policy Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Policy Type</em>' attribute.
	 * @see ODRLCommonVocabulary.PolicyType
	 * @see #setPolicyType(PolicyType)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy_PolicyType()
	 * @model default="Null" ordered="false"
	 * @generated
	 */
	PolicyType getPolicyType();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.ODRLPolicy#getPolicyType <em>Policy Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Policy Type</em>' attribute.
	 * @see ODRLCommonVocabulary.PolicyType
	 * @see #getPolicyType()
	 * @generated
	 */
	void setPolicyType(PolicyType value);

	/**
	 * Returns the value of the '<em><b>Profiles</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Profiles</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Profiles</em>' attribute list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy_Profiles()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getProfiles();

	/**
	 * Returns the value of the '<em><b>Inherits From</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Inherits From</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Inherits From</em>' attribute list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getODRLPolicy_InheritsFrom()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getInheritsFrom();

} // ODRLPolicy
