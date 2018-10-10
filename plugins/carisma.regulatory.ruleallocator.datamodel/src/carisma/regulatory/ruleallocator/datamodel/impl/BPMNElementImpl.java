/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>BPMN Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl#getName <em>Name</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl#getID <em>ID</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl#getType <em>Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl#getIncoming <em>Incoming</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl#getOutgoing <em>Outgoing</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl#getProcessId <em>Process Id</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BPMNElementImpl extends EObjectImpl implements BPMNElement {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getID() <em>ID</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getID()
	 * @generated
	 * @ordered
	 */
	protected static final String ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getID() <em>ID</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getID()
	 * @generated
	 * @ordered
	 */
	protected String id = ID_EDEFAULT;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected ModelElementType type;

	/**
	 * The cached value of the '{@link #getIncoming() <em>Incoming</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIncoming()
	 * @generated
	 * @ordered
	 */
	protected EList incoming;

	/**
	 * The cached value of the '{@link #getOutgoing() <em>Outgoing</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOutgoing()
	 * @generated
	 * @ordered
	 */
	protected EList outgoing;

	/**
	 * The default value of the '{@link #getProcessId() <em>Process Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProcessId()
	 * @generated
	 * @ordered
	 */
	protected static final String PROCESS_ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getProcessId() <em>Process Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProcessId()
	 * @generated
	 * @ordered
	 */
	protected String processId = PROCESS_ID_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BPMNElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return DatamodelPackage.Literals.BPMN_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setName(String newName) {
		String oldName = name;
		name = newName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.BPMN_ELEMENT__NAME, oldName, name));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getID() {
		return id;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setID(String newID) {
		String oldID = id;
		id = newID;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.BPMN_ELEMENT__ID, oldID, id));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelElementType getType() {
		if (type != null && type.eIsProxy()) {
			InternalEObject oldType = (InternalEObject)type;
			type = (ModelElementType)eResolveProxy(oldType);
			if (type != oldType) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, DatamodelPackage.BPMN_ELEMENT__TYPE, oldType, type));
			}
		}
		return type;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ModelElementType basicGetType() {
		return type;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setType(ModelElementType newType) {
		ModelElementType oldType = type;
		type = newType;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.BPMN_ELEMENT__TYPE, oldType, type));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getIncoming() {
		if (incoming == null) {
			incoming = new EDataTypeUniqueEList(String.class, this, DatamodelPackage.BPMN_ELEMENT__INCOMING);
		}
		return incoming;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getOutgoing() {
		if (outgoing == null) {
			outgoing = new EDataTypeUniqueEList(String.class, this, DatamodelPackage.BPMN_ELEMENT__OUTGOING);
		}
		return outgoing;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getProcessId() {
		return processId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setProcessId(String newProcessId) {
		String oldProcessId = processId;
		processId = newProcessId;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.BPMN_ELEMENT__PROCESS_ID, oldProcessId, processId));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DatamodelPackage.BPMN_ELEMENT__NAME:
				return getName();
			case DatamodelPackage.BPMN_ELEMENT__ID:
				return getID();
			case DatamodelPackage.BPMN_ELEMENT__TYPE:
				if (resolve) return getType();
				return basicGetType();
			case DatamodelPackage.BPMN_ELEMENT__INCOMING:
				return getIncoming();
			case DatamodelPackage.BPMN_ELEMENT__OUTGOING:
				return getOutgoing();
			case DatamodelPackage.BPMN_ELEMENT__PROCESS_ID:
				return getProcessId();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DatamodelPackage.BPMN_ELEMENT__NAME:
				setName((String)newValue);
				return;
			case DatamodelPackage.BPMN_ELEMENT__ID:
				setID((String)newValue);
				return;
			case DatamodelPackage.BPMN_ELEMENT__TYPE:
				setType((ModelElementType)newValue);
				return;
			case DatamodelPackage.BPMN_ELEMENT__INCOMING:
				getIncoming().clear();
				getIncoming().addAll((Collection)newValue);
				return;
			case DatamodelPackage.BPMN_ELEMENT__OUTGOING:
				getOutgoing().clear();
				getOutgoing().addAll((Collection)newValue);
				return;
			case DatamodelPackage.BPMN_ELEMENT__PROCESS_ID:
				setProcessId((String)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(int featureID) {
		switch (featureID) {
			case DatamodelPackage.BPMN_ELEMENT__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DatamodelPackage.BPMN_ELEMENT__ID:
				setID(ID_EDEFAULT);
				return;
			case DatamodelPackage.BPMN_ELEMENT__TYPE:
				setType((ModelElementType)null);
				return;
			case DatamodelPackage.BPMN_ELEMENT__INCOMING:
				getIncoming().clear();
				return;
			case DatamodelPackage.BPMN_ELEMENT__OUTGOING:
				getOutgoing().clear();
				return;
			case DatamodelPackage.BPMN_ELEMENT__PROCESS_ID:
				setProcessId(PROCESS_ID_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DatamodelPackage.BPMN_ELEMENT__NAME:
				return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
			case DatamodelPackage.BPMN_ELEMENT__ID:
				return ID_EDEFAULT == null ? id != null : !ID_EDEFAULT.equals(id);
			case DatamodelPackage.BPMN_ELEMENT__TYPE:
				return type != null;
			case DatamodelPackage.BPMN_ELEMENT__INCOMING:
				return incoming != null && !incoming.isEmpty();
			case DatamodelPackage.BPMN_ELEMENT__OUTGOING:
				return outgoing != null && !outgoing.isEmpty();
			case DatamodelPackage.BPMN_ELEMENT__PROCESS_ID:
				return PROCESS_ID_EDEFAULT == null ? processId != null : !PROCESS_ID_EDEFAULT.equals(processId);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", ID: ");
		result.append(id);
		result.append(", incoming: ");
		result.append(incoming);
		result.append(", outgoing: ");
		result.append(outgoing);
		result.append(", processId: ");
		result.append(processId);
		result.append(')');
		return result.toString();
	}

} //BPMNElementImpl
