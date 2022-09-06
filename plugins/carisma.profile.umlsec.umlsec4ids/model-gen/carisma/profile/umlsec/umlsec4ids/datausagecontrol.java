/**
 */
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>datausagecontrol</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol#getBase_ActivityPartition <em>Base Activity Partition</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol#getPermission <em>Permission</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol#getObligation_start <em>Obligation start</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol#getObligation_stop <em>Obligation stop</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol#getProhibition <em>Prohibition</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdatausagecontrol()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='Data Usage Control'"
 * @generated
 */
public interface datausagecontrol extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Activity Partition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Activity Partition</em>' reference.
	 * @see #setBase_ActivityPartition(ActivityPartition)
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdatausagecontrol_Base_ActivityPartition()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ActivityPartition getBase_ActivityPartition();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.umlsec4ids.datausagecontrol#getBase_ActivityPartition <em>Base Activity Partition</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Activity Partition</em>' reference.
	 * @see #getBase_ActivityPartition()
	 * @generated
	 */
	void setBase_ActivityPartition(ActivityPartition value);

	/**
	 * Returns the value of the '<em><b>Permission</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Permission</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdatausagecontrol_Permission()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getPermission();

	/**
	 * Returns the value of the '<em><b>Obligation start</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Obligation start</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdatausagecontrol_Obligation_start()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getObligation_start();

	/**
	 * Returns the value of the '<em><b>Obligation stop</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Obligation stop</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdatausagecontrol_Obligation_stop()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getObligation_stop();

	/**
	 * Returns the value of the '<em><b>Prohibition</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Prohibition</em>' reference list.
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getdatausagecontrol_Prohibition()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getProhibition();

} // datausagecontrol
