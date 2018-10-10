/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.fairexchange;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>fairexchange</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.fairexchangeImpl#getStart <em>Start</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.fairexchangeImpl#getStop <em>Stop</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.fairexchangeImpl#getAdversary <em>Adversary</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.fairexchangeImpl#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @generated
 */
public class fairexchangeImpl extends MinimalEObjectImpl.Container implements fairexchange {
	/**
	 * The cached value of the '{@link #getStart() <em>Start</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStart()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> start;

	/**
	 * The cached value of the '{@link #getStop() <em>Stop</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStop()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> stop;

	/**
	 * The default value of the '{@link #getAdversary() <em>Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdversary()
	 * @generated
	 * @ordered
	 */
	protected static final String ADVERSARY_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAdversary() <em>Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdversary()
	 * @generated
	 * @ordered
	 */
	protected String adversary = ADVERSARY_EDEFAULT;

	/**
	 * The cached value of the '{@link #getBase_Package() <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Package()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Package base_Package;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected fairexchangeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.FAIREXCHANGE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Action> getStart() {
		if (start == null) {
			start = new EObjectResolvingEList<Action>(Action.class, this, UmlsecPackage.FAIREXCHANGE__START);
		}
		return start;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getStart(String name) {
		return getStart(name, false, null);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getStart(String name, boolean ignoreCase, EClass eClass) {
		startLoop: for (Action start : getStart()) {
			if (eClass != null && !eClass.isInstance(start))
				continue startLoop;
			if (name != null && !(ignoreCase ? name.equalsIgnoreCase(start.getName()) : name.equals(start.getName())))
				continue startLoop;
			return start;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Action> getStop() {
		if (stop == null) {
			stop = new EObjectResolvingEList<Action>(Action.class, this, UmlsecPackage.FAIREXCHANGE__STOP);
		}
		return stop;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getStop(String name) {
		return getStop(name, false, null);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getStop(String name, boolean ignoreCase, EClass eClass) {
		stopLoop: for (Action stop : getStop()) {
			if (eClass != null && !eClass.isInstance(stop))
				continue stopLoop;
			if (name != null && !(ignoreCase ? name.equalsIgnoreCase(stop.getName()) : name.equals(stop.getName())))
				continue stopLoop;
			return stop;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAdversary() {
		return adversary;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAdversary(String newAdversary) {
		String oldAdversary = adversary;
		adversary = newAdversary;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.FAIREXCHANGE__ADVERSARY, oldAdversary, adversary));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Package getBase_Package() {
		if (base_Package != null && base_Package.eIsProxy()) {
			InternalEObject oldBase_Package = (InternalEObject)base_Package;
			base_Package = (org.eclipse.uml2.uml.Package)eResolveProxy(oldBase_Package);
			if (base_Package != oldBase_Package) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.FAIREXCHANGE__BASE_PACKAGE, oldBase_Package, base_Package));
			}
		}
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Package basicGetBase_Package() {
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Package(org.eclipse.uml2.uml.Package newBase_Package) {
		org.eclipse.uml2.uml.Package oldBase_Package = base_Package;
		base_Package = newBase_Package;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.FAIREXCHANGE__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.FAIREXCHANGE__START:
				return getStart();
			case UmlsecPackage.FAIREXCHANGE__STOP:
				return getStop();
			case UmlsecPackage.FAIREXCHANGE__ADVERSARY:
				return getAdversary();
			case UmlsecPackage.FAIREXCHANGE__BASE_PACKAGE:
				if (resolve) return getBase_Package();
				return basicGetBase_Package();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case UmlsecPackage.FAIREXCHANGE__START:
				getStart().clear();
				getStart().addAll((Collection<? extends Action>)newValue);
				return;
			case UmlsecPackage.FAIREXCHANGE__STOP:
				getStop().clear();
				getStop().addAll((Collection<? extends Action>)newValue);
				return;
			case UmlsecPackage.FAIREXCHANGE__ADVERSARY:
				setAdversary((String)newValue);
				return;
			case UmlsecPackage.FAIREXCHANGE__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)newValue);
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
			case UmlsecPackage.FAIREXCHANGE__START:
				getStart().clear();
				return;
			case UmlsecPackage.FAIREXCHANGE__STOP:
				getStop().clear();
				return;
			case UmlsecPackage.FAIREXCHANGE__ADVERSARY:
				setAdversary(ADVERSARY_EDEFAULT);
				return;
			case UmlsecPackage.FAIREXCHANGE__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)null);
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
			case UmlsecPackage.FAIREXCHANGE__START:
				return start != null && !start.isEmpty();
			case UmlsecPackage.FAIREXCHANGE__STOP:
				return stop != null && !stop.isEmpty();
			case UmlsecPackage.FAIREXCHANGE__ADVERSARY:
				return ADVERSARY_EDEFAULT == null ? adversary != null : !ADVERSARY_EDEFAULT.equals(adversary);
			case UmlsecPackage.FAIREXCHANGE__BASE_PACKAGE:
				return base_Package != null;
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

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (adversary: ");
		result.append(adversary);
		result.append(')');
		return result.toString();
	}

} //fairexchangeImpl
