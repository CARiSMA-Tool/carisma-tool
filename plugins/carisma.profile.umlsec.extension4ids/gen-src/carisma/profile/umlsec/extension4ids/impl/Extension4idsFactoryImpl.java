/**
 */
package carisma.profile.umlsec.extension4ids.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import carisma.profile.umlsec.extension4ids.*;

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
			case Extension4idsPackage.USAGE_CONTROL: return createUsageControl();
			case Extension4idsPackage.TRANSFER_PROCESS_PROTOCOL: return createTransferProcessProtocol();
			case Extension4idsPackage.PROVIDER_CONNECTOR: return createProviderConnector();
			case Extension4idsPackage.CONSUMER_CONNECTOR: return createConsumerConnector();
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
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
			case Extension4idsPackage.TRANSFER_TYPE:
				return createTransferTypeFromString(eDataType, initialValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
			case Extension4idsPackage.TRANSFER_TYPE:
				return convertTransferTypeToString(eDataType, instanceValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
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
	public UsageControl createUsageControl() {
		UsageControlImpl usageControl = new UsageControlImpl();
		return usageControl;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public TransferProcessProtocol createTransferProcessProtocol() {
		TransferProcessProtocolImpl transferProcessProtocol = new TransferProcessProtocolImpl();
		return transferProcessProtocol;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ProviderConnector createProviderConnector() {
		ProviderConnectorImpl providerConnector = new ProviderConnectorImpl();
		return providerConnector;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ConsumerConnector createConsumerConnector() {
		ConsumerConnectorImpl consumerConnector = new ConsumerConnectorImpl();
		return consumerConnector;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TransferType createTransferTypeFromString(EDataType eDataType, String initialValue) {
		TransferType result = TransferType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertTransferTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
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
