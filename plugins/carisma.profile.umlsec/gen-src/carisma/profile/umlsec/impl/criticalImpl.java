/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.critical;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;

import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Component;
import org.eclipse.uml2.uml.InstanceSpecification;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>critical</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getSecrecy <em>Secrecy</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getIntegrity <em>Integrity</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getHigh <em>High</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getBase_Class <em>Base Class</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getBase_Component <em>Base Component</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getFresh <em>Fresh</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getAuthenticity <em>Authenticity</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getBase_InstanceSpecification <em>Base Instance Specification</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getBase_Classifier <em>Base Classifier</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.criticalImpl#getPrivacy <em>Privacy</em>}</li>
 * </ul>
 *
 * @generated
 */
public class criticalImpl extends MinimalEObjectImpl.Container implements critical {
	/**
	 * The cached value of the '{@link #getSecrecy() <em>Secrecy</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSecrecy()
	 * @generated
	 * @ordered
	 */
	protected EList<String> secrecy;

	/**
	 * The cached value of the '{@link #getIntegrity() <em>Integrity</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntegrity()
	 * @generated
	 * @ordered
	 */
	protected EList<String> integrity;

	/**
	 * The cached value of the '{@link #getHigh() <em>High</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHigh()
	 * @generated
	 * @ordered
	 */
	protected EList<String> high;

	/**
	 * The cached value of the '{@link #getBase_Class() <em>Base Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Class()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Class base_Class;

	/**
	 * The cached value of the '{@link #getBase_Component() <em>Base Component</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Component()
	 * @generated
	 * @ordered
	 */
	protected Component base_Component;

	/**
	 * The cached value of the '{@link #getFresh() <em>Fresh</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFresh()
	 * @generated
	 * @ordered
	 */
	protected EList<String> fresh;

	/**
	 * The cached value of the '{@link #getAuthenticity() <em>Authenticity</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAuthenticity()
	 * @generated
	 * @ordered
	 */
	protected EList<String> authenticity;

	/**
	 * The cached value of the '{@link #getBase_InstanceSpecification() <em>Base Instance Specification</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_InstanceSpecification()
	 * @generated
	 * @ordered
	 */
	protected InstanceSpecification base_InstanceSpecification;

	/**
	 * The cached value of the '{@link #getBase_Classifier() <em>Base Classifier</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Classifier()
	 * @generated
	 * @ordered
	 */
	protected Classifier base_Classifier;

	/**
	 * The cached value of the '{@link #getPrivacy() <em>Privacy</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPrivacy()
	 * @generated
	 * @ordered
	 */
	protected EList<String> privacy;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected criticalImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.CRITICAL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getSecrecy() {
		if (secrecy == null) {
			secrecy = new EDataTypeEList<String>(String.class, this, UmlsecPackage.CRITICAL__SECRECY);
		}
		return secrecy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getIntegrity() {
		if (integrity == null) {
			integrity = new EDataTypeEList<String>(String.class, this, UmlsecPackage.CRITICAL__INTEGRITY);
		}
		return integrity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getHigh() {
		if (high == null) {
			high = new EDataTypeEList<String>(String.class, this, UmlsecPackage.CRITICAL__HIGH);
		}
		return high;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Class getBase_Class() {
		if (base_Class != null && base_Class.eIsProxy()) {
			InternalEObject oldBase_Class = (InternalEObject)base_Class;
			base_Class = (org.eclipse.uml2.uml.Class)eResolveProxy(oldBase_Class);
			if (base_Class != oldBase_Class) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.CRITICAL__BASE_CLASS, oldBase_Class, base_Class));
			}
		}
		return base_Class;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Class basicGetBase_Class() {
		return base_Class;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Class(org.eclipse.uml2.uml.Class newBase_Class) {
		org.eclipse.uml2.uml.Class oldBase_Class = base_Class;
		base_Class = newBase_Class;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.CRITICAL__BASE_CLASS, oldBase_Class, base_Class));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Component getBase_Component() {
		if (base_Component != null && base_Component.eIsProxy()) {
			InternalEObject oldBase_Component = (InternalEObject)base_Component;
			base_Component = (Component)eResolveProxy(oldBase_Component);
			if (base_Component != oldBase_Component) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.CRITICAL__BASE_COMPONENT, oldBase_Component, base_Component));
			}
		}
		return base_Component;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Component basicGetBase_Component() {
		return base_Component;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Component(Component newBase_Component) {
		Component oldBase_Component = base_Component;
		base_Component = newBase_Component;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.CRITICAL__BASE_COMPONENT, oldBase_Component, base_Component));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getFresh() {
		if (fresh == null) {
			fresh = new EDataTypeEList<String>(String.class, this, UmlsecPackage.CRITICAL__FRESH);
		}
		return fresh;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getAuthenticity() {
		if (authenticity == null) {
			authenticity = new EDataTypeEList<String>(String.class, this, UmlsecPackage.CRITICAL__AUTHENTICITY);
		}
		return authenticity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InstanceSpecification getBase_InstanceSpecification() {
		if (base_InstanceSpecification != null && base_InstanceSpecification.eIsProxy()) {
			InternalEObject oldBase_InstanceSpecification = (InternalEObject)base_InstanceSpecification;
			base_InstanceSpecification = (InstanceSpecification)eResolveProxy(oldBase_InstanceSpecification);
			if (base_InstanceSpecification != oldBase_InstanceSpecification) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.CRITICAL__BASE_INSTANCE_SPECIFICATION, oldBase_InstanceSpecification, base_InstanceSpecification));
			}
		}
		return base_InstanceSpecification;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InstanceSpecification basicGetBase_InstanceSpecification() {
		return base_InstanceSpecification;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_InstanceSpecification(InstanceSpecification newBase_InstanceSpecification) {
		InstanceSpecification oldBase_InstanceSpecification = base_InstanceSpecification;
		base_InstanceSpecification = newBase_InstanceSpecification;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.CRITICAL__BASE_INSTANCE_SPECIFICATION, oldBase_InstanceSpecification, base_InstanceSpecification));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Classifier getBase_Classifier() {
		if (base_Classifier != null && base_Classifier.eIsProxy()) {
			InternalEObject oldBase_Classifier = (InternalEObject)base_Classifier;
			base_Classifier = (Classifier)eResolveProxy(oldBase_Classifier);
			if (base_Classifier != oldBase_Classifier) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.CRITICAL__BASE_CLASSIFIER, oldBase_Classifier, base_Classifier));
			}
		}
		return base_Classifier;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Classifier basicGetBase_Classifier() {
		return base_Classifier;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Classifier(Classifier newBase_Classifier) {
		Classifier oldBase_Classifier = base_Classifier;
		base_Classifier = newBase_Classifier;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.CRITICAL__BASE_CLASSIFIER, oldBase_Classifier, base_Classifier));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getPrivacy() {
		if (privacy == null) {
			privacy = new EDataTypeEList<String>(String.class, this, UmlsecPackage.CRITICAL__PRIVACY);
		}
		return privacy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.CRITICAL__SECRECY:
				return getSecrecy();
			case UmlsecPackage.CRITICAL__INTEGRITY:
				return getIntegrity();
			case UmlsecPackage.CRITICAL__HIGH:
				return getHigh();
			case UmlsecPackage.CRITICAL__BASE_CLASS:
				if (resolve) return getBase_Class();
				return basicGetBase_Class();
			case UmlsecPackage.CRITICAL__BASE_COMPONENT:
				if (resolve) return getBase_Component();
				return basicGetBase_Component();
			case UmlsecPackage.CRITICAL__FRESH:
				return getFresh();
			case UmlsecPackage.CRITICAL__AUTHENTICITY:
				return getAuthenticity();
			case UmlsecPackage.CRITICAL__BASE_INSTANCE_SPECIFICATION:
				if (resolve) return getBase_InstanceSpecification();
				return basicGetBase_InstanceSpecification();
			case UmlsecPackage.CRITICAL__BASE_CLASSIFIER:
				if (resolve) return getBase_Classifier();
				return basicGetBase_Classifier();
			case UmlsecPackage.CRITICAL__PRIVACY:
				return getPrivacy();
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
			case UmlsecPackage.CRITICAL__SECRECY:
				getSecrecy().clear();
				getSecrecy().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.CRITICAL__INTEGRITY:
				getIntegrity().clear();
				getIntegrity().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.CRITICAL__HIGH:
				getHigh().clear();
				getHigh().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.CRITICAL__BASE_CLASS:
				setBase_Class((org.eclipse.uml2.uml.Class)newValue);
				return;
			case UmlsecPackage.CRITICAL__BASE_COMPONENT:
				setBase_Component((Component)newValue);
				return;
			case UmlsecPackage.CRITICAL__FRESH:
				getFresh().clear();
				getFresh().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.CRITICAL__AUTHENTICITY:
				getAuthenticity().clear();
				getAuthenticity().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.CRITICAL__BASE_INSTANCE_SPECIFICATION:
				setBase_InstanceSpecification((InstanceSpecification)newValue);
				return;
			case UmlsecPackage.CRITICAL__BASE_CLASSIFIER:
				setBase_Classifier((Classifier)newValue);
				return;
			case UmlsecPackage.CRITICAL__PRIVACY:
				getPrivacy().clear();
				getPrivacy().addAll((Collection<? extends String>)newValue);
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
			case UmlsecPackage.CRITICAL__SECRECY:
				getSecrecy().clear();
				return;
			case UmlsecPackage.CRITICAL__INTEGRITY:
				getIntegrity().clear();
				return;
			case UmlsecPackage.CRITICAL__HIGH:
				getHigh().clear();
				return;
			case UmlsecPackage.CRITICAL__BASE_CLASS:
				setBase_Class((org.eclipse.uml2.uml.Class)null);
				return;
			case UmlsecPackage.CRITICAL__BASE_COMPONENT:
				setBase_Component((Component)null);
				return;
			case UmlsecPackage.CRITICAL__FRESH:
				getFresh().clear();
				return;
			case UmlsecPackage.CRITICAL__AUTHENTICITY:
				getAuthenticity().clear();
				return;
			case UmlsecPackage.CRITICAL__BASE_INSTANCE_SPECIFICATION:
				setBase_InstanceSpecification((InstanceSpecification)null);
				return;
			case UmlsecPackage.CRITICAL__BASE_CLASSIFIER:
				setBase_Classifier((Classifier)null);
				return;
			case UmlsecPackage.CRITICAL__PRIVACY:
				getPrivacy().clear();
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
			case UmlsecPackage.CRITICAL__SECRECY:
				return secrecy != null && !secrecy.isEmpty();
			case UmlsecPackage.CRITICAL__INTEGRITY:
				return integrity != null && !integrity.isEmpty();
			case UmlsecPackage.CRITICAL__HIGH:
				return high != null && !high.isEmpty();
			case UmlsecPackage.CRITICAL__BASE_CLASS:
				return base_Class != null;
			case UmlsecPackage.CRITICAL__BASE_COMPONENT:
				return base_Component != null;
			case UmlsecPackage.CRITICAL__FRESH:
				return fresh != null && !fresh.isEmpty();
			case UmlsecPackage.CRITICAL__AUTHENTICITY:
				return authenticity != null && !authenticity.isEmpty();
			case UmlsecPackage.CRITICAL__BASE_INSTANCE_SPECIFICATION:
				return base_InstanceSpecification != null;
			case UmlsecPackage.CRITICAL__BASE_CLASSIFIER:
				return base_Classifier != null;
			case UmlsecPackage.CRITICAL__PRIVACY:
				return privacy != null && !privacy.isEmpty();
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
		result.append(" (secrecy: ");
		result.append(secrecy);
		result.append(", integrity: ");
		result.append(integrity);
		result.append(", high: ");
		result.append(high);
		result.append(", fresh: ");
		result.append(fresh);
		result.append(", authenticity: ");
		result.append(authenticity);
		result.append(", privacy: ");
		result.append(privacy);
		result.append(')');
		return result.toString();
	}

} //criticalImpl
