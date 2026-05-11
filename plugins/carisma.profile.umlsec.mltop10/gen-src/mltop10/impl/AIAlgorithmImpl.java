/**
 */
package mltop10.impl;

import mltop10.AIAlgorithm;
import mltop10.Mltop10Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>AI Algorithm</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.AIAlgorithmImpl#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.impl.AIAlgorithmImpl#isPublic <em>Public</em>}</li>
 *   <li>{@link mltop10.impl.AIAlgorithmImpl#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.impl.AIAlgorithmImpl#isRandomize <em>Randomize</em>}</li>
 *   <li>{@link mltop10.impl.AIAlgorithmImpl#isRegularisation <em>Regularisation</em>}</li>
 * </ul>
 *
 * @generated
 */
public class AIAlgorithmImpl extends MinimalEObjectImpl.Container implements AIAlgorithm {
	/**
	 * The cached value of the '{@link #getBase_Artifact() <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Artifact()
	 * @generated
	 * @ordered
	 */
	protected Artifact base_Artifact;

	/**
	 * The default value of the '{@link #isPublic() <em>Public</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPublic()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PUBLIC_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPublic() <em>Public</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPublic()
	 * @generated
	 * @ordered
	 */
	protected boolean public_ = PUBLIC_EDEFAULT;

	/**
	 * The default value of the '{@link #isAccessControl() <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAccessControl()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ACCESS_CONTROL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAccessControl() <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAccessControl()
	 * @generated
	 * @ordered
	 */
	protected boolean accessControl = ACCESS_CONTROL_EDEFAULT;

	/**
	 * The default value of the '{@link #isRandomize() <em>Randomize</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRandomize()
	 * @generated
	 * @ordered
	 */
	protected static final boolean RANDOMIZE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRandomize() <em>Randomize</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRandomize()
	 * @generated
	 * @ordered
	 */
	protected boolean randomize = RANDOMIZE_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularisation() <em>Regularisation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularisation()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULARISATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularisation() <em>Regularisation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularisation()
	 * @generated
	 * @ordered
	 */
	protected boolean regularisation = REGULARISATION_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AIAlgorithmImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.AI_ALGORITHM;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Artifact getBase_Artifact() {
		if (base_Artifact != null && base_Artifact.eIsProxy()) {
			InternalEObject oldBase_Artifact = (InternalEObject)base_Artifact;
			base_Artifact = (Artifact)eResolveProxy(oldBase_Artifact);
			if (base_Artifact != oldBase_Artifact) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.AI_ALGORITHM__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
			}
		}
		return base_Artifact;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Artifact basicGetBase_Artifact() {
		return base_Artifact;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Artifact(Artifact newBase_Artifact) {
		Artifact oldBase_Artifact = base_Artifact;
		base_Artifact = newBase_Artifact;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_ALGORITHM__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isPublic() {
		return public_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setPublic(boolean newPublic) {
		boolean oldPublic = public_;
		public_ = newPublic;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_ALGORITHM__PUBLIC, oldPublic, public_));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isAccessControl() {
		return accessControl;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setAccessControl(boolean newAccessControl) {
		boolean oldAccessControl = accessControl;
		accessControl = newAccessControl;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_ALGORITHM__ACCESS_CONTROL, oldAccessControl, accessControl));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRandomize() {
		return randomize;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRandomize(boolean newRandomize) {
		boolean oldRandomize = randomize;
		randomize = newRandomize;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_ALGORITHM__RANDOMIZE, oldRandomize, randomize));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularisation() {
		return regularisation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularisation(boolean newRegularisation) {
		boolean oldRegularisation = regularisation;
		regularisation = newRegularisation;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_ALGORITHM__REGULARISATION, oldRegularisation, regularisation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.AI_ALGORITHM__BASE_ARTIFACT:
				if (resolve) return getBase_Artifact();
				return basicGetBase_Artifact();
			case Mltop10Package.AI_ALGORITHM__PUBLIC:
				return isPublic();
			case Mltop10Package.AI_ALGORITHM__ACCESS_CONTROL:
				return isAccessControl();
			case Mltop10Package.AI_ALGORITHM__RANDOMIZE:
				return isRandomize();
			case Mltop10Package.AI_ALGORITHM__REGULARISATION:
				return isRegularisation();
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
			case Mltop10Package.AI_ALGORITHM__BASE_ARTIFACT:
				setBase_Artifact((Artifact)newValue);
				return;
			case Mltop10Package.AI_ALGORITHM__PUBLIC:
				setPublic((Boolean)newValue);
				return;
			case Mltop10Package.AI_ALGORITHM__ACCESS_CONTROL:
				setAccessControl((Boolean)newValue);
				return;
			case Mltop10Package.AI_ALGORITHM__RANDOMIZE:
				setRandomize((Boolean)newValue);
				return;
			case Mltop10Package.AI_ALGORITHM__REGULARISATION:
				setRegularisation((Boolean)newValue);
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
			case Mltop10Package.AI_ALGORITHM__BASE_ARTIFACT:
				setBase_Artifact((Artifact)null);
				return;
			case Mltop10Package.AI_ALGORITHM__PUBLIC:
				setPublic(PUBLIC_EDEFAULT);
				return;
			case Mltop10Package.AI_ALGORITHM__ACCESS_CONTROL:
				setAccessControl(ACCESS_CONTROL_EDEFAULT);
				return;
			case Mltop10Package.AI_ALGORITHM__RANDOMIZE:
				setRandomize(RANDOMIZE_EDEFAULT);
				return;
			case Mltop10Package.AI_ALGORITHM__REGULARISATION:
				setRegularisation(REGULARISATION_EDEFAULT);
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
			case Mltop10Package.AI_ALGORITHM__BASE_ARTIFACT:
				return base_Artifact != null;
			case Mltop10Package.AI_ALGORITHM__PUBLIC:
				return public_ != PUBLIC_EDEFAULT;
			case Mltop10Package.AI_ALGORITHM__ACCESS_CONTROL:
				return accessControl != ACCESS_CONTROL_EDEFAULT;
			case Mltop10Package.AI_ALGORITHM__RANDOMIZE:
				return randomize != RANDOMIZE_EDEFAULT;
			case Mltop10Package.AI_ALGORITHM__REGULARISATION:
				return regularisation != REGULARISATION_EDEFAULT;
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
		result.append(" (Public: ");
		result.append(public_);
		result.append(", AccessControl: ");
		result.append(accessControl);
		result.append(", Randomize: ");
		result.append(randomize);
		result.append(", Regularisation: ");
		result.append(regularisation);
		result.append(')');
		return result.toString();
	}

} //AIAlgorithmImpl
