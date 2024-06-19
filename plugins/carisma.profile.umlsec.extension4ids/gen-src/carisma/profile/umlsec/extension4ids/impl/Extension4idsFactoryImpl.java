/**
 */
package carisma.profile.umlsec.extension4ids.impl;

import carisma.profile.umlsec.extension4ids.*;

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
public class Extension4idsFactoryImpl extends EFactoryImpl implements Extension4idsFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static Extension4idsFactory init() {
		try {
			Extension4idsFactory theExtension4idsFactory = (Extension4idsFactory)EPackage.Registry.INSTANCE.getEFactory(Extension4idsPackage.eNS_URI);
			if (theExtension4idsFactory != null) {
				return theExtension4idsFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new Extension4idsFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Extension4idsFactoryImpl() {
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
			case Extension4idsPackage.ID_SCONNECTOR: return createIDSconnector();
			case Extension4idsPackage.USAGECONTROL: return createusagecontrol();
			case Extension4idsPackage.DATATRANSFER: return createdatatransfer();
			case Extension4idsPackage.PROVIDER: return createprovider();
			case Extension4idsPackage.CONSUMER: return createconsumer();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public IDSconnector createIDSconnector() {
		IDSconnectorImpl idSconnector = new IDSconnectorImpl();
		return idSconnector;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public usagecontrol createusagecontrol() {
		usagecontrolImpl usagecontrol = new usagecontrolImpl();
		return usagecontrol;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public datatransfer createdatatransfer() {
		datatransferImpl datatransfer = new datatransferImpl();
		return datatransfer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public provider createprovider() {
		providerImpl provider = new providerImpl();
		return provider;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public consumer createconsumer() {
		consumerImpl consumer = new consumerImpl();
		return consumer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Extension4idsPackage getExtension4idsPackage() {
		return (Extension4idsPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static Extension4idsPackage getPackage() {
		return Extension4idsPackage.eINSTANCE;
	}

} //Extension4idsFactoryImpl
