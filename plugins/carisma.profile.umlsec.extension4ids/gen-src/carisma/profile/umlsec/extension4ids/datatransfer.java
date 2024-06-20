/**
 */
package carisma.profile.umlsec.extension4ids;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Message;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>datatransfer</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getType <em>Type</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_req_step <em>Transfer req step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_start_step <em>Transfer start step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getPush_pull_step <em>Push pull step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_complete_step <em>Transfer complete step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_suspend_step <em>Transfer suspend step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_terminate_step <em>Transfer terminate step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.datatransfer#getBase_Interaction <em>Base Interaction</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='data transfer'"
 * @generated
 */
public interface datatransfer extends EObject {
	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see #setType(String)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Type()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	String getType();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see #getType()
	 * @generated
	 */
	void setType(String value);

	/**
	 * Returns the value of the '<em><b>Transfer req step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transfer req step</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transfer req step</em>' reference.
	 * @see #setTransfer_req_step(Message)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Transfer_req_step()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Message getTransfer_req_step();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_req_step <em>Transfer req step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Transfer req step</em>' reference.
	 * @see #getTransfer_req_step()
	 * @generated
	 */
	void setTransfer_req_step(Message value);

	/**
	 * Returns the value of the '<em><b>Transfer start step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transfer start step</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transfer start step</em>' reference.
	 * @see #setTransfer_start_step(Message)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Transfer_start_step()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Message getTransfer_start_step();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_start_step <em>Transfer start step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Transfer start step</em>' reference.
	 * @see #getTransfer_start_step()
	 * @generated
	 */
	void setTransfer_start_step(Message value);

	/**
	 * Returns the value of the '<em><b>Push pull step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Push pull step</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Push pull step</em>' reference.
	 * @see #setPush_pull_step(Message)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Push_pull_step()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Message getPush_pull_step();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getPush_pull_step <em>Push pull step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Push pull step</em>' reference.
	 * @see #getPush_pull_step()
	 * @generated
	 */
	void setPush_pull_step(Message value);

	/**
	 * Returns the value of the '<em><b>Transfer complete step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transfer complete step</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transfer complete step</em>' reference.
	 * @see #setTransfer_complete_step(Message)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Transfer_complete_step()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Message getTransfer_complete_step();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_complete_step <em>Transfer complete step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Transfer complete step</em>' reference.
	 * @see #getTransfer_complete_step()
	 * @generated
	 */
	void setTransfer_complete_step(Message value);

	/**
	 * Returns the value of the '<em><b>Transfer suspend step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transfer suspend step</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transfer suspend step</em>' reference.
	 * @see #setTransfer_suspend_step(Message)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Transfer_suspend_step()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Message getTransfer_suspend_step();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_suspend_step <em>Transfer suspend step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Transfer suspend step</em>' reference.
	 * @see #getTransfer_suspend_step()
	 * @generated
	 */
	void setTransfer_suspend_step(Message value);

	/**
	 * Returns the value of the '<em><b>Transfer terminate step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transfer terminate step</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transfer terminate step</em>' reference.
	 * @see #setTransfer_terminate_step(Message)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Transfer_terminate_step()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Message getTransfer_terminate_step();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_terminate_step <em>Transfer terminate step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Transfer terminate step</em>' reference.
	 * @see #getTransfer_terminate_step()
	 * @generated
	 */
	void setTransfer_terminate_step(Message value);

	/**
	 * Returns the value of the '<em><b>Base Interaction</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Interaction</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Interaction</em>' reference.
	 * @see #setBase_Interaction(Interaction)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getdatatransfer_Base_Interaction()
	 * @model ordered="false"
	 * @generated
	 */
	Interaction getBase_Interaction();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.datatransfer#getBase_Interaction <em>Base Interaction</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Interaction</em>' reference.
	 * @see #getBase_Interaction()
	 * @generated
	 */
	void setBase_Interaction(Interaction value);

} // datatransfer
