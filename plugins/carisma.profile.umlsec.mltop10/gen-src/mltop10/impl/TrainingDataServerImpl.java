/**
 */
package mltop10.impl;

import mltop10.Mltop10Package;
import mltop10.TrainingDataServer;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Node;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Training Data Server</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.TrainingDataServerImpl#getBase_Node <em>Base Node</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataServerImpl#isSecureDataStorage <em>Secure Data Storage</em>}</li>
 * </ul>
 *
 * @generated
 */
public class TrainingDataServerImpl extends MinimalEObjectImpl.Container implements TrainingDataServer {
	/**
	 * The cached value of the '{@link #getBase_Node() <em>Base Node</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Node()
	 * @generated
	 * @ordered
	 */
	protected Node base_Node;

	/**
	 * The default value of the '{@link #isSecureDataStorage() <em>Secure Data Storage</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSecureDataStorage()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SECURE_DATA_STORAGE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSecureDataStorage() <em>Secure Data Storage</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSecureDataStorage()
	 * @generated
	 * @ordered
	 */
	protected boolean secureDataStorage = SECURE_DATA_STORAGE_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TrainingDataServerImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.TRAINING_DATA_SERVER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Node getBase_Node() {
		if (base_Node != null && base_Node.eIsProxy()) {
			InternalEObject oldBase_Node = (InternalEObject)base_Node;
			base_Node = (Node)eResolveProxy(oldBase_Node);
			if (base_Node != oldBase_Node) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.TRAINING_DATA_SERVER__BASE_NODE, oldBase_Node, base_Node));
			}
		}
		return base_Node;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Node basicGetBase_Node() {
		return base_Node;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Node(Node newBase_Node) {
		Node oldBase_Node = base_Node;
		base_Node = newBase_Node;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA_SERVER__BASE_NODE, oldBase_Node, base_Node));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isSecureDataStorage() {
		return secureDataStorage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setSecureDataStorage(boolean newSecureDataStorage) {
		boolean oldSecureDataStorage = secureDataStorage;
		secureDataStorage = newSecureDataStorage;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA_SERVER__SECURE_DATA_STORAGE, oldSecureDataStorage, secureDataStorage));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.TRAINING_DATA_SERVER__BASE_NODE:
				if (resolve) return getBase_Node();
				return basicGetBase_Node();
			case Mltop10Package.TRAINING_DATA_SERVER__SECURE_DATA_STORAGE:
				return isSecureDataStorage();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case Mltop10Package.TRAINING_DATA_SERVER__BASE_NODE:
				setBase_Node((Node)newValue);
				return;
			case Mltop10Package.TRAINING_DATA_SERVER__SECURE_DATA_STORAGE:
				setSecureDataStorage((Boolean)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case Mltop10Package.TRAINING_DATA_SERVER__BASE_NODE:
				setBase_Node((Node)null);
				return;
			case Mltop10Package.TRAINING_DATA_SERVER__SECURE_DATA_STORAGE:
				setSecureDataStorage(SECURE_DATA_STORAGE_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case Mltop10Package.TRAINING_DATA_SERVER__BASE_NODE:
				return base_Node != null;
			case Mltop10Package.TRAINING_DATA_SERVER__SECURE_DATA_STORAGE:
				return secureDataStorage != SECURE_DATA_STORAGE_EDEFAULT;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuilder result = new StringBuilder(super.toString());
		result.append(" (SecureDataStorage: ");
		result.append(secureDataStorage);
		result.append(')');
		return result.toString();
	}

} //TrainingDataServerImpl
