/**
 */
package carisma.profile.umlsec.umlsec4ids.impl;

import carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage;
import carisma.profile.umlsec.umlsec4ids.X509;
import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Node;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>X509</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.X509Impl#getBase_Node <em>Base Node</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.X509Impl#getExpiration_date_yyyy_mm_dd <em>Expiration date yyyy mm dd</em>}</li>
 * </ul>
 *
 * @generated
 */
public class X509Impl extends MinimalEObjectImpl.Container implements X509 {
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
	 * The default value of the '{@link #getExpiration_date_yyyy_mm_dd() <em>Expiration date yyyy mm dd</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExpiration_date_yyyy_mm_dd()
	 * @generated
	 * @ordered
	 */
	protected static final int EXPIRATION_DATE_YYYY_MM_DD_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getExpiration_date_yyyy_mm_dd() <em>Expiration date yyyy mm dd</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExpiration_date_yyyy_mm_dd()
	 * @generated
	 * @ordered
	 */
	protected int expiration_date_yyyy_mm_dd = EXPIRATION_DATE_YYYY_MM_DD_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected X509Impl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Umlsec4idsPackage.Literals.X509;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Umlsec4idsPackage.X509__BASE_NODE, oldBase_Node, base_Node));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Umlsec4idsPackage.X509__BASE_NODE, oldBase_Node, base_Node));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int getExpiration_date_yyyy_mm_dd() {
		return expiration_date_yyyy_mm_dd;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setExpiration_date_yyyy_mm_dd(int newExpiration_date_yyyy_mm_dd) {
		int oldExpiration_date_yyyy_mm_dd = expiration_date_yyyy_mm_dd;
		expiration_date_yyyy_mm_dd = newExpiration_date_yyyy_mm_dd;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Umlsec4idsPackage.X509__EXPIRATION_DATE_YYYY_MM_DD, oldExpiration_date_yyyy_mm_dd, expiration_date_yyyy_mm_dd));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Umlsec4idsPackage.X509__BASE_NODE:
				if (resolve) return getBase_Node();
				return basicGetBase_Node();
			case Umlsec4idsPackage.X509__EXPIRATION_DATE_YYYY_MM_DD:
				return getExpiration_date_yyyy_mm_dd();
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
			case Umlsec4idsPackage.X509__BASE_NODE:
				setBase_Node((Node)newValue);
				return;
			case Umlsec4idsPackage.X509__EXPIRATION_DATE_YYYY_MM_DD:
				setExpiration_date_yyyy_mm_dd((Integer)newValue);
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
			case Umlsec4idsPackage.X509__BASE_NODE:
				setBase_Node((Node)null);
				return;
			case Umlsec4idsPackage.X509__EXPIRATION_DATE_YYYY_MM_DD:
				setExpiration_date_yyyy_mm_dd(EXPIRATION_DATE_YYYY_MM_DD_EDEFAULT);
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
			case Umlsec4idsPackage.X509__BASE_NODE:
				return base_Node != null;
			case Umlsec4idsPackage.X509__EXPIRATION_DATE_YYYY_MM_DD:
				return expiration_date_yyyy_mm_dd != EXPIRATION_DATE_YYYY_MM_DD_EDEFAULT;
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
		result.append(" (expiration_date_yyyy_mm_dd: ");
		result.append(expiration_date_yyyy_mm_dd);
		result.append(')');
		return result.toString();
	}

} //X509Impl
