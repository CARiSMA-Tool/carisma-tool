/**
 */
package UMLsecenc.impl;

import UMLsecenc.*;

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
public class UMLsecencFactoryImpl extends EFactoryImpl implements UMLsecencFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static UMLsecencFactory init() {
		try {
			UMLsecencFactory theUMLsecencFactory = (UMLsecencFactory)EPackage.Registry.INSTANCE.getEFactory(UMLsecencPackage.eNS_URI);
			if (theUMLsecencFactory != null) {
				return theUMLsecencFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new UMLsecencFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UMLsecencFactoryImpl() {
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
			case UMLsecencPackage.SECURELINKSENC: return createsecurelinksenc();
			case UMLsecencPackage.SECRECYENC: return createsecrecyenc();
			case UMLsecencPackage.ENCRYPTEDENC: return createencryptedenc();
			case UMLsecencPackage.ENCRYPTEDPERSISTENCE: return createencryptedpersistence();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public securelinksenc createsecurelinksenc() {
		securelinksencImpl securelinksenc = new securelinksencImpl();
		return securelinksenc;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public secrecyenc createsecrecyenc() {
		secrecyencImpl secrecyenc = new secrecyencImpl();
		return secrecyenc;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public encryptedenc createencryptedenc() {
		encryptedencImpl encryptedenc = new encryptedencImpl();
		return encryptedenc;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public encryptedpersistence createencryptedpersistence() {
		encryptedpersistenceImpl encryptedpersistence = new encryptedpersistenceImpl();
		return encryptedpersistence;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public UMLsecencPackage getUMLsecencPackage() {
		return (UMLsecencPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static UMLsecencPackage getPackage() {
		return UMLsecencPackage.eINSTANCE;
	}

} //UMLsecencFactoryImpl
