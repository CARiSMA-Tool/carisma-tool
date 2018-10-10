/**
 */
package carisma.profile.umlsec.enc.impl;

import carisma.profile.umlsec.enc.*;

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
public class EncFactoryImpl extends EFactoryImpl implements EncFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static EncFactory init() {
		try {
			EncFactory theEncFactory = (EncFactory)EPackage.Registry.INSTANCE.getEFactory(EncPackage.eNS_URI);
			if (theEncFactory != null) {
				return theEncFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new EncFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EncFactoryImpl() {
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
			case EncPackage.SECURELINKSENC: return createsecurelinksenc();
			case EncPackage.SECRECYENC: return createsecrecyenc();
			case EncPackage.ENCRYPTEDENC: return createencryptedenc();
			case EncPackage.ENCRYPTEDPERSISTENCE: return createencryptedpersistence();
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
	public EncPackage getEncPackage() {
		return (EncPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static EncPackage getPackage() {
		return EncPackage.eINSTANCE;
	}

} //EncFactoryImpl
