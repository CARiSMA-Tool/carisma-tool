/**
 */
package mltop10.impl;

import mltop10.Mltop10Package;
import mltop10.SecureAIScenario;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Model;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Secure AI Scenario</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#isPackageIntegrityVerified <em>Package Integrity Verified</em>}</li>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#isPackagesFromSecureSources <em>Packages From Secure Sources</em>}</li>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#isRegularSecurityAudits <em>Regular Security Audits</em>}</li>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#isRegularPackageUpdates <em>Regular Package Updates</em>}</li>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#isSecureDeployment <em>Secure Deployment</em>}</li>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link mltop10.impl.SecureAIScenarioImpl#getBase_Model <em>Base Model</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SecureAIScenarioImpl extends MinimalEObjectImpl.Container implements SecureAIScenario {
	/**
	 * The default value of the '{@link #isPackageIntegrityVerified() <em>Package Integrity Verified</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPackageIntegrityVerified()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PACKAGE_INTEGRITY_VERIFIED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPackageIntegrityVerified() <em>Package Integrity Verified</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPackageIntegrityVerified()
	 * @generated
	 * @ordered
	 */
	protected boolean packageIntegrityVerified = PACKAGE_INTEGRITY_VERIFIED_EDEFAULT;

	/**
	 * The default value of the '{@link #isPackagesFromSecureSources() <em>Packages From Secure Sources</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPackagesFromSecureSources()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PACKAGES_FROM_SECURE_SOURCES_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPackagesFromSecureSources() <em>Packages From Secure Sources</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPackagesFromSecureSources()
	 * @generated
	 * @ordered
	 */
	protected boolean packagesFromSecureSources = PACKAGES_FROM_SECURE_SOURCES_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularSecurityAudits() <em>Regular Security Audits</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularSecurityAudits()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_SECURITY_AUDITS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularSecurityAudits() <em>Regular Security Audits</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularSecurityAudits()
	 * @generated
	 * @ordered
	 */
	protected boolean regularSecurityAudits = REGULAR_SECURITY_AUDITS_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularPackageUpdates() <em>Regular Package Updates</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularPackageUpdates()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_PACKAGE_UPDATES_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularPackageUpdates() <em>Regular Package Updates</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularPackageUpdates()
	 * @generated
	 * @ordered
	 */
	protected boolean regularPackageUpdates = REGULAR_PACKAGE_UPDATES_EDEFAULT;

	/**
	 * The default value of the '{@link #isSecureDeployment() <em>Secure Deployment</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSecureDeployment()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SECURE_DEPLOYMENT_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSecureDeployment() <em>Secure Deployment</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSecureDeployment()
	 * @generated
	 * @ordered
	 */
	protected boolean secureDeployment = SECURE_DEPLOYMENT_EDEFAULT;

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
	 * The cached value of the '{@link #getBase_Model() <em>Base Model</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Model()
	 * @generated
	 * @ordered
	 */
	protected Model base_Model;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SecureAIScenarioImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.SECURE_AI_SCENARIO;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isPackageIntegrityVerified() {
		return packageIntegrityVerified;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setPackageIntegrityVerified(boolean newPackageIntegrityVerified) {
		boolean oldPackageIntegrityVerified = packageIntegrityVerified;
		packageIntegrityVerified = newPackageIntegrityVerified;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED, oldPackageIntegrityVerified, packageIntegrityVerified));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isPackagesFromSecureSources() {
		return packagesFromSecureSources;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setPackagesFromSecureSources(boolean newPackagesFromSecureSources) {
		boolean oldPackagesFromSecureSources = packagesFromSecureSources;
		packagesFromSecureSources = newPackagesFromSecureSources;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES, oldPackagesFromSecureSources, packagesFromSecureSources));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularSecurityAudits() {
		return regularSecurityAudits;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularSecurityAudits(boolean newRegularSecurityAudits) {
		boolean oldRegularSecurityAudits = regularSecurityAudits;
		regularSecurityAudits = newRegularSecurityAudits;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS, oldRegularSecurityAudits, regularSecurityAudits));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularPackageUpdates() {
		return regularPackageUpdates;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularPackageUpdates(boolean newRegularPackageUpdates) {
		boolean oldRegularPackageUpdates = regularPackageUpdates;
		regularPackageUpdates = newRegularPackageUpdates;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES, oldRegularPackageUpdates, regularPackageUpdates));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isSecureDeployment() {
		return secureDeployment;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setSecureDeployment(boolean newSecureDeployment) {
		boolean oldSecureDeployment = secureDeployment;
		secureDeployment = newSecureDeployment;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__SECURE_DEPLOYMENT, oldSecureDeployment, secureDeployment));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public org.eclipse.uml2.uml.Package getBase_Package() {
		if (base_Package != null && base_Package.eIsProxy()) {
			InternalEObject oldBase_Package = (InternalEObject)base_Package;
			base_Package = (org.eclipse.uml2.uml.Package)eResolveProxy(oldBase_Package);
			if (base_Package != oldBase_Package) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.SECURE_AI_SCENARIO__BASE_PACKAGE, oldBase_Package, base_Package));
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
	@Override
	public void setBase_Package(org.eclipse.uml2.uml.Package newBase_Package) {
		org.eclipse.uml2.uml.Package oldBase_Package = base_Package;
		base_Package = newBase_Package;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Model getBase_Model() {
		if (base_Model != null && base_Model.eIsProxy()) {
			InternalEObject oldBase_Model = (InternalEObject)base_Model;
			base_Model = (Model)eResolveProxy(oldBase_Model);
			if (base_Model != oldBase_Model) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.SECURE_AI_SCENARIO__BASE_MODEL, oldBase_Model, base_Model));
			}
		}
		return base_Model;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Model basicGetBase_Model() {
		return base_Model;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Model(Model newBase_Model) {
		Model oldBase_Model = base_Model;
		base_Model = newBase_Model;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_AI_SCENARIO__BASE_MODEL, oldBase_Model, base_Model));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED:
				return isPackageIntegrityVerified();
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES:
				return isPackagesFromSecureSources();
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS:
				return isRegularSecurityAudits();
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES:
				return isRegularPackageUpdates();
			case Mltop10Package.SECURE_AI_SCENARIO__SECURE_DEPLOYMENT:
				return isSecureDeployment();
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_PACKAGE:
				if (resolve) return getBase_Package();
				return basicGetBase_Package();
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_MODEL:
				if (resolve) return getBase_Model();
				return basicGetBase_Model();
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
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED:
				setPackageIntegrityVerified((Boolean)newValue);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES:
				setPackagesFromSecureSources((Boolean)newValue);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS:
				setRegularSecurityAudits((Boolean)newValue);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES:
				setRegularPackageUpdates((Boolean)newValue);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__SECURE_DEPLOYMENT:
				setSecureDeployment((Boolean)newValue);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)newValue);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_MODEL:
				setBase_Model((Model)newValue);
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
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED:
				setPackageIntegrityVerified(PACKAGE_INTEGRITY_VERIFIED_EDEFAULT);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES:
				setPackagesFromSecureSources(PACKAGES_FROM_SECURE_SOURCES_EDEFAULT);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS:
				setRegularSecurityAudits(REGULAR_SECURITY_AUDITS_EDEFAULT);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES:
				setRegularPackageUpdates(REGULAR_PACKAGE_UPDATES_EDEFAULT);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__SECURE_DEPLOYMENT:
				setSecureDeployment(SECURE_DEPLOYMENT_EDEFAULT);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)null);
				return;
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_MODEL:
				setBase_Model((Model)null);
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
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED:
				return packageIntegrityVerified != PACKAGE_INTEGRITY_VERIFIED_EDEFAULT;
			case Mltop10Package.SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES:
				return packagesFromSecureSources != PACKAGES_FROM_SECURE_SOURCES_EDEFAULT;
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS:
				return regularSecurityAudits != REGULAR_SECURITY_AUDITS_EDEFAULT;
			case Mltop10Package.SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES:
				return regularPackageUpdates != REGULAR_PACKAGE_UPDATES_EDEFAULT;
			case Mltop10Package.SECURE_AI_SCENARIO__SECURE_DEPLOYMENT:
				return secureDeployment != SECURE_DEPLOYMENT_EDEFAULT;
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_PACKAGE:
				return base_Package != null;
			case Mltop10Package.SECURE_AI_SCENARIO__BASE_MODEL:
				return base_Model != null;
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
		result.append(" (PackageIntegrityVerified: ");
		result.append(packageIntegrityVerified);
		result.append(", PackagesFromSecureSources: ");
		result.append(packagesFromSecureSources);
		result.append(", RegularSecurityAudits: ");
		result.append(regularSecurityAudits);
		result.append(", RegularPackageUpdates: ");
		result.append(regularPackageUpdates);
		result.append(", SecureDeployment: ");
		result.append(secureDeployment);
		result.append(')');
		return result.toString();
	}

} //SecureAIScenarioImpl
