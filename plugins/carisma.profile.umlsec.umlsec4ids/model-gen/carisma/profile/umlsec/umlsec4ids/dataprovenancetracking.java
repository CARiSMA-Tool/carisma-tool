/**
 */
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>dataprovenancetracking</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getStart_action <em>Start action</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getStop_action <em>Stop action</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getProtected <em>Protected</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getRight <em>Right</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getClearing_house <em>Clearing house</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='Data Provenance Tracking'"
 * @generated
 */
public interface dataprovenancetracking extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Package</em>' reference.
	 * @see #setBase_Package(org.eclipse.uml2.uml.Package)
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking_Base_Package()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.umlsec4ids.dataprovenancetracking#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

	/**
	 * Returns the value of the '<em><b>Start action</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Start action</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking_Start_action()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getStart_action();

	/**
	 * Returns the value of the '<em><b>Stop action</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Stop action</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking_Stop_action()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getStop_action();

	/**
	 * Returns the value of the '<em><b>Protected</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Protected</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking_Protected()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getProtected();

	/**
	 * Returns the value of the '<em><b>Right</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking_Right()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getRight();

	/**
	 * Returns the value of the '<em><b>Clearing house</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.ActivityPartition}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Clearing house</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdataprovenancetracking_Clearing_house()
	 * @model required="true" ordered="false"
	 *        annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='Clearing House'"
	 * @generated
	 */
	EList<ActivityPartition> getClearing_house();

} // dataprovenancetracking
