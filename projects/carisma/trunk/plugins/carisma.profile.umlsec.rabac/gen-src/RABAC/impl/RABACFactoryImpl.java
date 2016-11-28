/**
 */
package RABAC.impl;

import RABAC.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class RABACFactoryImpl extends EFactoryImpl implements RABACFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static RABACFactory init() {
		try {
			RABACFactory theRABACFactory = (RABACFactory)EPackage.Registry.INSTANCE.getEFactory(RABACPackage.eNS_URI);
			if (theRABACFactory != null) {
				return theRABACFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new RABACFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RABACFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case RABACPackage.ABAC: return createabac();
			case RABACPackage.ABAC_ATTRIBUTE: return createabacAttribute();
			case RABACPackage.ABAC_REQUIRE: return createabacRequire();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public abac createabac() {
		abacImpl abac = new abacImpl();
		return abac;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public abacAttribute createabacAttribute() {
		abacAttributeImpl abacAttribute = new abacAttributeImpl();
		return abacAttribute;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public abacRequire createabacRequire() {
		abacRequireImpl abacRequire = new abacRequireImpl();
		return abacRequire;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RABACPackage getRABACPackage() {
		return (RABACPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static RABACPackage getPackage() {
		return RABACPackage.eINSTANCE;
	}

} //RABACFactoryImpl
