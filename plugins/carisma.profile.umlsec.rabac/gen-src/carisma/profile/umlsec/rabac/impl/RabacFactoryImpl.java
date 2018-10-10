/**
 */
package carisma.profile.umlsec.rabac.impl;

import carisma.profile.umlsec.rabac.*;

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
public class RabacFactoryImpl extends EFactoryImpl implements RabacFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static RabacFactory init() {
		try {
			RabacFactory theRabacFactory = (RabacFactory)EPackage.Registry.INSTANCE.getEFactory(RabacPackage.eNS_URI);
			if (theRabacFactory != null) {
				return theRabacFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new RabacFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RabacFactoryImpl() {
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
			case RabacPackage.ABAC: return createabac();
			case RabacPackage.ABAC_ATTRIBUTE: return createabacAttribute();
			case RabacPackage.ABAC_REQUIRE: return createabacRequire();
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
	public RabacPackage getRabacPackage() {
		return (RabacPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static RabacPackage getPackage() {
		return RabacPackage.eINSTANCE;
	}

} //RabacFactoryImpl
