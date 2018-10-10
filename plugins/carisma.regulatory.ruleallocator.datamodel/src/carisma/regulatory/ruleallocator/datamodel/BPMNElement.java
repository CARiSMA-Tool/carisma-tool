/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>BPMN Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getName <em>Name</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getID <em>ID</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getType <em>Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getIncoming <em>Incoming</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getOutgoing <em>Outgoing</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getProcessId <em>Process Id</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement()
 * @model
 * @generated
 */
public interface BPMNElement extends EObject {
	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>ID</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>ID</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>ID</em>' attribute.
	 * @see #setID(String)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement_ID()
	 * @model required="true"
	 * @generated
	 */
	String getID();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getID <em>ID</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>ID</em>' attribute.
	 * @see #getID()
	 * @generated
	 */
	void setID(String value);

	/**
	 * Returns the value of the '<em><b>Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' reference.
	 * @see #setType(ModelElementType)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement_Type()
	 * @model required="true"
	 * @generated
	 */
	ModelElementType getType();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getType <em>Type</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' reference.
	 * @see #getType()
	 * @generated
	 */
	void setType(ModelElementType value);

	/**
	 * Returns the value of the '<em><b>Incoming</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Incoming</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Incoming</em>' attribute list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement_Incoming()
	 * @model
	 * @generated
	 */
	EList getIncoming();

	/**
	 * Returns the value of the '<em><b>Outgoing</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Outgoing</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Outgoing</em>' attribute list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement_Outgoing()
	 * @model
	 * @generated
	 */
	EList getOutgoing();

	/**
	 * Returns the value of the '<em><b>Process Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Process Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Process Id</em>' attribute.
	 * @see #setProcessId(String)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getBPMNElement_ProcessId()
	 * @model
	 * @generated
	 */
	String getProcessId();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getProcessId <em>Process Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Process Id</em>' attribute.
	 * @see #getProcessId()
	 * @generated
	 */
	void setProcessId(String value);

} // BPMNElement
