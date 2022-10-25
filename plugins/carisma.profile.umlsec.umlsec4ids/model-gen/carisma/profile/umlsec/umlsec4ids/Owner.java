/**
 */
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Owner</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.Owner#getBase_ActivityPartition <em>Base Activity Partition</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.Owner#getProtected <em>Protected</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.Owner#getRequested_attributes <em>Requested attributes</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.Owner#getRequested_actions <em>Requested actions</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getOwner()
 * @model
 * @generated
 */
public interface Owner extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Activity Partition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Activity Partition</em>' reference.
	 * @see #setBase_ActivityPartition(ActivityPartition)
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getOwner_Base_ActivityPartition()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ActivityPartition getBase_ActivityPartition();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.umlsec4ids.Owner#getBase_ActivityPartition <em>Base Activity Partition</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Activity Partition</em>' reference.
	 * @see #getBase_ActivityPartition()
	 * @generated
	 */
	void setBase_ActivityPartition(ActivityPartition value);

	/**
	 * Returns the value of the '<em><b>Protected</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Protected</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getOwner_Protected()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getProtected();

	/**
	 * Returns the value of the '<em><b>Requested attributes</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Requested attributes</em>' attribute list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getOwner_Requested_attributes()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	EList<String> getRequested_attributes();

	/**
	 * Returns the value of the '<em><b>Requested actions</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Requested actions</em>' attribute list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getOwner_Requested_actions()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	EList<String> getRequested_actions();

} // Owner
